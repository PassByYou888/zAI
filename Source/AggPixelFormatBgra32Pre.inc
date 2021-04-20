//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixBgraPre(p: PAggOrderBgra; CR, Cg, CB, alpha: Cardinal); overload;
begin
  alpha := CAggBaseMask - alpha;

  p^.r := Int8u(((p^.r * alpha) shr CAggBaseShift) + CR);
  p^.g := Int8u(((p^.g * alpha) shr CAggBaseShift) + Cg);
  p^.b := Int8u(((p^.b * alpha) shr CAggBaseShift) + CB);
  p^.a := Int8u(CAggBaseMask - ((alpha * (CAggBaseMask - p^.a)) shr CAggBaseShift));
end;

procedure BlendPixBgraPre(p: PAggOrderBgra; CR, Cg, CB, alpha, Cover: Cardinal);
  overload;
begin
  alpha := CAggBaseMask - alpha;
  Cover := (Cover + 1) shl (CAggBaseShift - 8);

  p^.r := Int8u((p^.r * alpha + CR * Cover) shr CAggBaseShift);
  p^.g := Int8u((p^.g * alpha + Cg * Cover) shr CAggBaseShift);
  p^.b := Int8u((p^.b * alpha + CB * Cover) shr CAggBaseShift);
  p^.a := Int8u(CAggBaseMask - ((alpha * (CAggBaseMask - p^.a)) shr
    CAggBaseShift));
end;

procedure CopyOrBlendPixBgraPre(p: PAggOrderBgra; CR, Cg, CB,
  alpha: Cardinal); inline; overload;
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
      BlendPixBgraPre(p, CR, Cg, CB, alpha);
end;

procedure CopyOrBlendPixBgraPre(p: PAggOrderBgra; CR, Cg, CB, alpha,
  Cover: Cardinal); inline; overload;
begin
  if Cover = 255 then
    CopyOrBlendPixBgraPre(p, CR, Cg, CB, alpha)
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
      BlendPixBgraPre(p, CR, Cg, CB, alpha, Cover);
  end;
end;

procedure Bgra32PreBlendPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor; Cover: Int8u);
var
  p: PAggOrderBgra;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(y));
  inc(p, x);

  CopyOrBlendPixBgraPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, c.Rgba8.a, Cover);
end;

procedure Bgra32PreBlendHorizontalLine(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
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

      Fill32Bit(p^, Len, v);
    end
    else if Cover = 255 then
      repeat
        BlendPixBgraPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);

        inc(p);
        dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgraPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);

        inc(p);
        dec(Len);
      until Len = 0;
  end;
end;

procedure Bgra32PreBlendVerticalLine(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
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
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixBgraPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);

        p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

        dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgraPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);

        p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

        dec(Len);
      until Len = 0;
  end;
end;

procedure Bgra32PreBlendSolidHorizontalSpan(This: TAggPixelFormatProcessor;
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
        BlendPixBgraPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);

      inc(p);
      inc(PtrComp(Covers));

      dec(Len);
    until Len = 0;
  end;
end;

procedure Bgra32PreBlendSolidVerticalSpan(This: TAggPixelFormatProcessor;
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
        BlendPixBgraPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);

      p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(Covers);
      dec(Len);
    until Len = 0;
  end;
end;

procedure Bgra32PreBlendColorHorizontalSpan(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u;
  Cover: Int8u);
var
  p: PAggOrderBgra;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(y));
  inc(p, x);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgraPre(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a, Covers^);

      inc(Covers);
      inc(p);
      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgraPre(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a);

      inc(p);
      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgraPre(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a, Cover);

      inc(p);
      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0;
end;

procedure Bgra32PreBlendColorVerticalSpan(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u;
  Cover: Int8u);
var
  p: PAggOrderBgra;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(y));
  inc(p, x);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgraPre(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a, Covers^);

      inc(Covers);

      p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgraPre(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a);

      p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgraPre(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a, Cover);

      p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0;
end;

procedure Bgra32PreBlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Source, dest: PAggOrderBgra;
  Incp: Integer;
begin
  Source := PAggOrderBgra(SourcePtr);
  dest := PAggOrderBgra(This.RenderingBuffer.Row(Ydst));
  inc(dest, Xdst);
  Incp := 4;

  if PtrComp(Xdst) > PtrComp(Xsrc) then
  begin
    inc(Source, Len - 1);
    inc(dest, Len - 1);

    Incp := -4;
  end;

  if Cover = 255 then
    repeat
      CopyOrBlendPixBgraPre(dest, Source^.r, Source^.g, Source^.b, Source^.a);

      inc(PtrComp(Source), Incp);
      inc(PtrComp(dest), Incp);
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgraPre(dest, Source^.r, Source^.g, Source^.b, Source^.a,
        Cover);

      inc(PtrComp(Source), Incp);
      inc(PtrComp(dest), Incp);
      dec(Len);
    until Len = 0;
end;

procedure Bgra32PreBlendFromColor(This, From: TAggPixelFormatProcessor;
  COLOR: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PixWidth: Cardinal;

  Source: PInt8u;
  dest: PAggOrderBgra;
begin
  PixWidth := From.PixWidth;
  Source := From.GetRowPointer(Ysrc);

  if Source <> nil then
  begin
    dest := PAggOrderBgra(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    inc(dest, Xdst);

    repeat
      CopyOrBlendPixBgraPre(dest, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
        COLOR.Rgba8.a, ShrInt32(Source^ * Cover + CAggBaseMask, CAggBaseShift));

      inc(PtrComp(Source), PixWidth);
      inc(PtrComp(dest), 4);
      dec(Len);
    until Len = 0;
  end;
end;

procedure Bgra32PreBlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
 PixWidth : Cardinal;

 Source : PInt8u;
 dest : PAggOrderBgra;

 COLOR : PAggColor;
begin
  PixWidth := From.PixWidth;
  Source := From.GetRowPointer(Ysrc);

  if Source <> nil then
  begin
    dest := PAggOrderBgra(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    inc(dest, Xdst);

    if Cover = 255 then
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + Source^ * SizeOf(TAggColor));

        CopyOrBlendPixBgraPre(dest, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.a);

        inc(PtrComp(Source), PixWidth);
        inc(dest);
        dec(Len);
      until Len = 0
    else
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + Source^ * SizeOf(TAggColor));

        CopyOrBlendPixBgraPre(dest, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.a, Cover);

        inc(PtrComp(Source), PixWidth);
        inc(dest);
        dec(Len);
      until Len = 0;
  end;
end; 
 
 
 
