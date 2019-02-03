//
// AggPas 2.4 RM3 pixel format definition file
//

function MakePix555(r, g, b: Cardinal): Int16u;
begin
  Result := Int16u(((r and $F8) shl 7) or ((g and $F8) shl 2) or (b shr 3)
    or $8000);
end;

procedure MakeColor555(var COLOR: TAggColor; p: PInt16u);
begin
  COLOR.FromRgbaInteger((p^ shr 7) and $F8, (p^ shr 2) and $F8, (p^ shl 3)
    and $F8);
end;

procedure BlendPix555(p: PInt16u; CR, Cg, CB, alpha: Cardinal);
var
  RGB: Int16u;
  r, g, b: Integer;
begin
  RGB := p^;

  r := (RGB shr 7) and $F8;
  g := (RGB shr 2) and $F8;
  b := (RGB shl 3) and $F8;

  p^ := Int16u(((((CR - r) * alpha + (r shl 8)) shr 1) and $7C00) or
    ((((Cg - g) * alpha + (g shl 8)) shr 6) and $03E0) or
    (((CB - b) * alpha + (b shl 8)) shr 11) or $8000);
end;

procedure CopyOrBlendPix555(p: PInt16u; c: PAggColor; Cover: Cardinal);
var
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    alpha := (c.Rgba8.a * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
      p^ := MakePix555(c.Rgba8.r, c.Rgba8.g, c.Rgba8.b)
    else
      BlendPix555(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);
  end;
end;

procedure Rgb555CopyPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor);
begin
  PInt16u(PtrComp(This.RenderingBuffer.Row(y)) + x * SizeOf(Int16u))^ :=
    MakePix555(c.Rgba8.r, c.Rgba8.g, c.Rgba8.b);
end;

procedure Rgb555BlendPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPix555(PInt16u(PtrComp(This.RenderingBuffer.Row(y)) +
    x * SizeOf(Int16u)), c, Cover);
end;

function Rgb555Pixel(This: TAggPixelFormatProcessor; x, y: Integer): TAggColor;
begin
  MakeColor555(Result, PInt16u(PtrComp(This.RenderingBuffer.Row(y)) + x *
    SizeOf(Int16u)));
end;

procedure Rgb555CopyHorizontalLine(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; c: PAggColor);
var
  p: PInt16u;
  v: Int16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(y)) + x * SizeOf(Int16u));
  v := MakePix555(c.Rgba8.r, c.Rgba8.g, c.Rgba8.b);

  repeat
    p^ := v;

    inc(PtrComp(p), SizeOf(Int16u));
    dec(Len);
  until Len = 0;
end;

procedure Rgb555CopyVerticalLine(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor);
var
  p: PInt16u;
  v: Int16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(y)) + x * SizeOf(Int16u));
  v := MakePix555(c.Rgba8.r, c.Rgba8.g, c.Rgba8.b);

  repeat
    p^ := v;
    p := PInt16u(This.RenderingBuffer.NextRow(PInt8u(p)));
    dec(Len);
  until Len = 0;
end;

procedure Rgb555BlendHorizontalLine(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
var
  p: PInt16u;
  v: Int16u;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PInt16u(PtrComp(This.RenderingBuffer.Row(y)) + x * SizeOf(Int16u));

    alpha := (c.Rgba8.a * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      v := MakePix555(c.Rgba8.r, c.Rgba8.g, c.Rgba8.b);

      repeat
        p^ := v;

        inc(PtrComp(p), SizeOf(Int16u));
        dec(Len);
      until Len = 0;
    end
    else
      repeat
        BlendPix555(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);

        inc(PtrComp(p), SizeOf(Int16u));
        dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb555BlendVerticalLine(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor; Cover: Int8u);
var
  p: PInt16u;
  v: Int16u;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PInt16u(PtrComp(This.RenderingBuffer.Row(y)) + x * SizeOf(Int16u));

    alpha := (c.Rgba8.a * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      v := MakePix555(c.Rgba8.r, c.Rgba8.g, c.Rgba8.b);

      repeat
        p^ := v;
        p := PInt16u(This.RenderingBuffer.NextRow(PInt8u(p)));

        dec(Len);
      until Len = 0;
    end
    else
      repeat
        BlendPix555(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);

        p := PInt16u(This.RenderingBuffer.NextRow(PInt8u(p)));

        dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb555BlendSolidHSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor; Covers: PInt8u);
var
  p: PInt16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(y)) + x * SizeOf(Int16u));

  repeat
    CopyOrBlendPix555(p, c, Covers^);

    inc(PtrComp(Covers));
    inc(PtrComp(p), SizeOf(Int16u));
    dec(Len);
  until Len = 0;
end;

procedure Rgb555BlendSolidVSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor; Covers: PInt8u);
var
  p: PInt16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(y)) + x * SizeOf(Int16u));

  repeat
    CopyOrBlendPix555(p, c, Covers^);

    inc(PtrComp(Covers));

    p := PInt16u(This.RenderingBuffer.NextRow(PInt8u(p)));

    dec(Len);
  until Len = 0;
end;

procedure Rgb555BlendColorHSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(y)) + x * SizeOf(Int16u));

  repeat
    if Covers <> nil then
    begin
      CopyOrBlendPix555(p, Colors, Covers^);

      inc(PtrComp(Covers), SizeOf(Int8u));

    end
    else
      CopyOrBlendPix555(p, Colors, Cover);

    inc(PtrComp(p), SizeOf(Int16u));
    inc(PtrComp(Colors), SizeOf(TAggColor));
    dec(Len);
  until Len = 0;
end;

procedure Rgb555BlendColorVSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(y)) + x * SizeOf(Int16u));

  repeat
    if Covers <> nil then
    begin
      CopyOrBlendPix555(p, Colors, Covers^);

      inc(PtrComp(Covers), SizeOf(Int8u));
    end
    else
      CopyOrBlendPix555(p, Colors, Cover);

    p := PInt16u(This.RenderingBuffer.NextRow(PInt8u(p)));

    inc(PtrComp(Colors), SizeOf(TAggColor));
    dec(Len);
  until Len = 0;
end;

procedure Rgb555CopyFrom(This: TAggPixelFormatProcessor;
  From: TAggRenderingBuffer; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal);
begin
  Move(PInt16u(PtrComp(From.Row(Ysrc)) + Xsrc * SizeOf(Int16u))^,
    PInt16u(PtrComp(This.RenderingBuffer.Row(Ydst)) + Xdst * SizeOf(Int16))^,
    Len * SizeOf(Int16u));
end;

procedure Rgb555BlendFrom(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; SourcePtr: PInt8u; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  PDst: PInt16u;
  alpha: Cardinal;
begin
  PDst := PInt16u(PtrComp(This.RenderingBuffer.Row(Ydst)) +
    Xdst * SizeOf(Int16u));

  repeat
    alpha := PInt8u(PtrComp(SourcePtr) + From.Order.a)^;

    if alpha <> 0 then
      if (alpha = CAggBaseMask) and (Cover = 255) then
        PDst^ := MakePix555(PInt8u(PtrComp(SourcePtr) + From.Order.r)^,
          PInt8u(PtrComp(SourcePtr) + From.Order.g)^,
          PInt8u(PtrComp(SourcePtr) + From.Order.b)^)
      else
        BlendPix555(PDst, PInt8u(PtrComp(SourcePtr) + From.Order.r)^,
          PInt8u(PtrComp(SourcePtr) + From.Order.g)^,
          PInt8u(PtrComp(SourcePtr) + From.Order.b)^, alpha);

    inc(PtrComp(SourcePtr), 4);
    inc(PtrComp(PDst), SizeOf(Int16u));
    dec(Len);
  until Len = 0;
end;

procedure Rgb555CopyColorHSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  p: PInt16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(y)) + x * SizeOf(Int16u));

  repeat
    p^ := MakePix555(Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b);

    inc(PtrComp(p), SizeOf(Int16u));
    inc(PtrComp(Colors), SizeOf(TAggColor));
    dec(Len);
  until Len = 0;
end;

procedure Rgb555CopyColorVSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  p: PInt16u;
begin
  p := PInt16u(PtrComp(This.RenderingBuffer.Row(y)) + x * SizeOf(Int16u));

  repeat
    p^ := MakePix555(Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b);
    p := PInt16u(This.RenderingBuffer.NextRow(PInt8u(p)));

    inc(PtrComp(Colors), SizeOf(TAggColor));
    dec(Len);
  until Len = 0;
end;

procedure Rgb555BlendFromColor(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; COLOR: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  Ppsz: Cardinal;
  PSrc: PInt8u;
  PDst: PInt16u;
begin
  Ppsz := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PInt16u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      Xdst * SizeOf(Int16u));

    repeat
      BlendPix555(PDst, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
        ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));

      inc(PtrComp(PSrc), Ppsz);
      inc(PtrComp(PDst), SizeOf(Int16u));
      dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb555BlendFromLUT(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; ColorLUT: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  Ppsz: Cardinal;
  PSrc: PInt8u;
  PDst: PInt16u;
  COLOR: PAggColor;
begin
  Ppsz := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PInt16u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      Xdst * SizeOf(Int16u));

    repeat
      COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

      BlendPix555(PDst, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
        ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));

      inc(PtrComp(PSrc), Ppsz);
      inc(PtrComp(PDst), SizeOf(Int16u));
      dec(Len);
    until Len = 0;
  end;
end; 
 
 
 
