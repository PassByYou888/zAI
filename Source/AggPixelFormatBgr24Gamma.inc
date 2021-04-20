//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixBgrGamma(Gamma: TAggGamma; p: PAggOrderBgr;
  CR, Cg, CB, alpha: Integer; Cover: Cardinal = 0);
var
  r, g, b: Cardinal;
begin
  r := Gamma.dir[TAggOrderBGR(Pointer(p)^).r];
  g := Gamma.dir[TAggOrderBGR(Pointer(p)^).g];
  b := Gamma.dir[TAggOrderBGR(Pointer(p)^).b];

  p^.r := Int8u(Gamma.Inv[(((Integer(Gamma.dir[CR]) - r) * alpha)
    shr CAggBaseShift) + r]);
  p^.g := Int8u(Gamma.Inv[(((Integer(Gamma.dir[Cg]) - g) * alpha)
    shr CAggBaseShift) + g]);
  p^.b := Int8u(Gamma.Inv[(((Integer(Gamma.dir[CB]) - b) * alpha)
    shr CAggBaseShift) + b]);
end;

procedure CopyOrBlendPixBgrGamma(Gamma: TAggGamma; p: PAggOrderBgr;
  c: PAggColor; Cover: Cardinal); overload;
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
      BlendPixBgrGamma(Gamma, p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);
  end;
end;

procedure CopyOrBlendPixBgrGamma(Gamma: TAggGamma; p: PAggOrderBgr;
  c: PAggColor); overload;
begin
  if c.Rgba8.a <> 0 then
    if c.Rgba8.a = CAggBaseMask then
    begin
      p^.r := c.Rgba8.r;
      p^.g := c.Rgba8.g;
      p^.b := c.Rgba8.b;
    end
    else
      BlendPixBgrGamma(Gamma, p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, c.Rgba8.a);
end;

procedure Bgr24GammaBlendPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor; Cover: Int8u);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(y));
  inc(p, x);
  CopyOrBlendPixBgrGamma(This.Gamma, p, c, Cover);
end;

procedure Bgr24GammaBlendHorizontalLine(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
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
        BlendPixBgrGamma(This.Gamma, p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);

        inc(PtrComp(p), 3);
        dec(Len);
      until Len = 0;
  end;
end;

procedure Bgr24GammaBlendVerticalLine(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
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
        BlendPixBgrGamma(This.Gamma, p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha,
          Cover);

        p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

        dec(Len);
      until Len = 0;
  end;
end;

procedure Bgr24GammaBlendSolidHorizontalSpan(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor; Covers: PInt8u);
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
        BlendPixBgrGamma(This.Gamma, p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b,
          alpha, Covers^);

      inc(PtrComp(p), 3);
      inc(PtrComp(Covers));
      dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24GammaBlendSolidVerticalSpan(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor; Covers: PInt8u);
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
        BlendPixBgrGamma(This.Gamma, p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha,
          Covers^);

      p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(PtrComp(Covers));
      dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24GammaBlendColorHorizontalSpan(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(y));
  inc(p, x);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, p, Colors, Covers^);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(Covers), SizeOf(Int8u));
      inc(PtrComp(p), 3);
      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, p, Colors);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(p), 3);
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, p, Colors, Cover);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(p), 3);
      dec(Len);
    until Len = 0;
end;

procedure Bgr24GammaBlendColorVerticalSpan(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(y));
  inc(p, x);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, p, Colors, Covers^);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(Covers);

      p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, p, Colors);

      inc(PtrComp(Colors), SizeOf(TAggColor));

      p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, p, Colors, Cover);

      inc(PtrComp(Colors), SizeOf(TAggColor));

      p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

      dec(Len);
    until Len = 0;
end;

procedure Bgr24GammaBlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PSrc, PDst: PAggOrderBgr;

  COLOR: TAggColor;
  alpha: Cardinal;
begin
  PSrc := PAggOrderBgr(SourcePtr);
  PDst := PAggOrderBgr(This.RenderingBuffer.Row(Ydst));
  inc(PDst, Xdst);

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
          BlendPixBgrGamma(This.Gamma, PDst,
            PInt8u(PtrComp(PSrc) + From.Order.r)^,
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

      CopyOrBlendPixBgrGamma(This.Gamma, PDst, @COLOR, Cover);

      inc(PtrComp(PSrc), 4);
      inc(PtrComp(PDst), 3);
      dec(Len);
    until Len = 0;
end;

procedure Bgr24GammaBlendFromColor(This, From: TAggPixelFormatProcessor;
  COLOR: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PixWidth: Cardinal;
  PSrc: PInt8u;
  PDst: PAggOrderBgr;
begin
  PixWidth := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PAggOrderBgr(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    inc(PDst, Xdst);
    repeat
      CopyOrBlendPixBgrGamma(This.Gamma, PDst, COLOR,
        ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));

      inc(PtrComp(PSrc), PixWidth);
      inc(PtrComp(PDst), 3);
      dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24GammaBlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PixWidth: Cardinal;
  PSrc: PInt8u;
  PDst: PAggOrderBgr;
  COLOR: PAggColor;
begin
  PixWidth := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PAggOrderBgr(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    inc(PDst, Xdst);

    if Cover = 255 then
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        BlendPixBgrGamma(This.Gamma, PDst, COLOR.Rgba8.r, COLOR.Rgba8.g,
          COLOR.Rgba8.b, COLOR.Rgba8.a);

        inc(PtrComp(PSrc), PixWidth);
        inc(PtrComp(PDst), 3);
        dec(Len);
      until Len = 0
    else
      repeat
        CopyOrBlendPixBgrGamma(This.Gamma, PDst,
          PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor)), Cover);

        inc(PtrComp(PSrc), PixWidth);
        inc(PtrComp(PDst), 3);
        dec(Len);
      until Len = 0;
  end;
end; 
 
 
 
