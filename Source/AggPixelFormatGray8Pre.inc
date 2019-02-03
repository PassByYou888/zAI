//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixGrayPre(p: PInt8u; cv, alpha, Cover: Cardinal); overload;
begin
  alpha := CAggBaseMask - alpha;
  Cover := (Cover + 1) shl (CAggBaseShift - 8);

  p^ := Int8u((p^ * alpha + cv * Cover) shr CAggBaseShift);
end;

procedure BlendPixGrayPre(p: PInt8u; cv, alpha: Cardinal); overload;
begin
  p^ := Int8u(((p^ * (CAggBaseMask - alpha)) shr CAggBaseShift) + cv);
end;

procedure CopyOrBlendPixGrayPre(p: PInt8u; c: PAggColor;
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
      BlendPixGrayPre(p, c.v, alpha, Cover);
  end;
end;

procedure CopyOrBlendPixPre(p: PInt8u; c: PAggColor); overload;
begin
  if c.Rgba8.a <> 0 then
    if c.Rgba8.a = CAggBaseMask then
      p^ := c.v
    else
      BlendPixGrayPre(p, c.v, c.Rgba8.a);
end;

procedure Gray8PreBlendPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixGrayPre(PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x *
    This.Step + This.Offset), c, Cover);
end;

procedure Gray8PreBlendHorizontalLine(This: TAggPixelFormatProcessor; x,
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
        BlendPixGrayPre(p, c.v, alpha, Cover);

        inc(PtrComp(p), This.Step);
        dec(Len);
      until Len = 0;
  end;
end;

procedure Gray8PreBlendVerticalLine(This: TAggPixelFormatProcessor; x, y: Integer;
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
        BlendPixGrayPre(p, c.v, alpha, Cover);

        p := This.RenderingBuffer.NextRow(p);

        dec(Len);
      until Len = 0;
  end;
end;

procedure Gray8PreBlendSolidHSpan(This: TAggPixelFormatProcessor; x, y: Integer;
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
        BlendPixGrayPre(p, c.v, alpha, Covers^);

      inc(PtrComp(p), This.Step);
      inc(PtrComp(Covers));
      dec(Len);
    until Len = 0;
  end;
end;

procedure Gray8PreBlendSolidVSpan(This: TAggPixelFormatProcessor; x, y: Integer;
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
        BlendPixGrayPre(p, c.v, alpha, Covers^);

      p := This.RenderingBuffer.NextRow(p);

      inc(PtrComp(Covers), SizeOf(Int8u));
      dec(Len);
    until Len = 0;
  end;
end;

procedure Gray8PreBlendColorHSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x * This.Step +
    This.Offset);

  if Covers <> nil then
    repeat
      CopyOrBlendPixGrayPre(p, Colors, Covers^);

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
        CopyOrBlendPixPre(p, Colors);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(p), This.Step);
      dec(Len);

    until Len = 0
  else
    repeat
      CopyOrBlendPixGrayPre(p, Colors, Cover);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(p), This.Step);
      dec(Len);
    until Len = 0;
end;

procedure Gray8PreBlendColorVSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x * This.Step +
    This.Offset);

  if Covers <> nil then
    repeat
      CopyOrBlendPixGrayPre(p, Colors, Covers^);

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
        CopyOrBlendPixPre(p, Colors);

      p := This.RenderingBuffer.NextRow(p);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixGrayPre(p, Colors, Cover);

      inc(PtrComp(Colors), SizeOf(TAggColor));

      p := This.RenderingBuffer.NextRow(p);

      dec(Len);
    until Len = 0;
end;

procedure Gray8PreBlendFromColor(This, From: TAggPixelFormatProcessor;
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
    PDst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) + Xdst
      * This.Step + This.Offset);

    repeat
      CopyOrBlendPixGrayPre(PDst, COLOR, ShrInt32(PSrc^ * Cover + CAggBaseMask,
        CAggBaseShift));

      inc(PtrComp(PSrc), Ppsz);
      inc(PtrComp(PDst), This.Step);
      dec(Len);
    until Len = 0;
  end;
end;

procedure Gray8PreBlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Ppsz: Cardinal;

  PSrc, PDst: PInt8u;
begin
  Ppsz := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) + Xdst
      * This.Step + This.Offset);

    repeat
      CopyOrBlendPixGrayPre(PDst,
        PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor)), Cover);

      inc(PtrComp(PSrc), Ppsz);
      inc(PtrComp(PDst), This.Step);
      dec(Len);
    until Len = 0;
  end;
end; 
 
 
 
