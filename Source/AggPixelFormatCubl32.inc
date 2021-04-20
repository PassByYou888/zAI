//
// AggPas 2.4 RM3 pixel format definition file
//

procedure CublCopyPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor);
begin
  This.Blender(This, This.BlendMode,
    PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2) * SizeOf(Int8u)),
    c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, c.Rgba8.a, 255);
end;

procedure CublBlendPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor;
  Cover: Int8u);
begin
  This.Blender(This, This.BlendMode,
    PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2) * SizeOf(Int8u)),
    c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, c.Rgba8.a, Cover);
end;

function CublPixel(This: TAggPixelFormatProcessor; x, y: Integer): TAggColor;
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2) * SizeOf(Int8u));

  Result.FromRgbaInteger(PInt8u(PtrComp(p) + This.Order.r)^,
    PInt8u(PtrComp(p) + This.Order.g)^, PInt8u(PtrComp(p) + This.Order.b)^,
    PInt8u(PtrComp(p) + This.Order.a)^);
end;

procedure CublCopyHorizontalLine(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2) * SizeOf(Int8u));

  repeat
    This.Blender(This, This.BlendMode, p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b,
      c.Rgba8.a, 255);

    inc(PtrComp(p), 4);
    dec(Len);
  until Len = 0;
end;

procedure CublCopyVerticalLine(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2) * SizeOf(Int8u));

  repeat
    This.Blender(This, This.BlendMode, p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b,
      c.Rgba8.a, 255);

    p := This.RenderingBuffer.NextRow(p);

    dec(Len);
  until Len = 0;
end;

procedure CublBlendHorizontalLine(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2) * SizeOf(Int8u));

  repeat
    This.Blender(This, This.BlendMode, p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b,
      c.Rgba8.a, Cover);

    inc(PtrComp(p), 4);
    dec(Len);
  until Len = 0;
end;

procedure CublBlendVerticalLine(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2) * SizeOf(Int8u));

  repeat
    This.Blender(This, This.BlendMode, p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b,
      c.Rgba8.a, Cover);

    p := This.RenderingBuffer.NextRow(p);

    dec(Len);
  until Len = 0;
end;

procedure CublBlendSolidHSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor; Covers: PInt8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2) * SizeOf(Int8u));

  repeat
    This.Blender(This, This.BlendMode, p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b,
      c.Rgba8.a, Covers^);

    inc(PtrComp(Covers), SizeOf(Int8u));
    inc(PtrComp(p), 4);
    dec(Len);
  until Len = 0;
end;

procedure CublBlendSolidVSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor; Covers: PInt8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2) * SizeOf(Int8u));

  repeat
    This.Blender(This, This.BlendMode, p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b,
      c.Rgba8.a, Covers^);

    inc(PtrComp(Covers), SizeOf(Int8u));

    p := This.RenderingBuffer.NextRow(p);

    dec(Len);
  until Len = 0;
end;

procedure CublBlendColorHSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2) * SizeOf(Int8u));

  repeat
    if Covers <> nil then
    begin
      This.Blender(This, This.BlendMode, p, Colors.Rgba8.r, Colors.Rgba8.g,
        Colors.Rgba8.b, Colors.Rgba8.a, Covers^);

      inc(PtrComp(Covers), SizeOf(Int8u));

    end
    else
      This.Blender(This, This.BlendMode, p, Colors.Rgba8.r, Colors.Rgba8.g,
        Colors.Rgba8.b, Colors.Rgba8.a, Cover);

    inc(PtrComp(p), 4);
    inc(PtrComp(Colors), SizeOf(TAggColor));
    dec(Len);
  until Len = 0;
end;

procedure CublBlendColorVSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2) * SizeOf(Int8u));

  repeat
    if Covers <> nil then
    begin
      This.Blender(This, This.BlendMode, p, Colors.Rgba8.r, Colors.Rgba8.g,
        Colors.Rgba8.b, Colors.Rgba8.a, Covers^);

      inc(PtrComp(Covers), SizeOf(Int8u));

    end
    else
      This.Blender(This, This.BlendMode, p, Colors.Rgba8.r, Colors.Rgba8.g,
        Colors.Rgba8.b, Colors.Rgba8.a, Cover);

    p := This.RenderingBuffer.NextRow(p);

    inc(PtrComp(Colors), SizeOf(TAggColor));
    dec(Len);
  until Len = 0;
end;

procedure CublBlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PSrc, PDst: PInt8u;
  Incp      : Integer;
begin
  PSrc := SourcePtr;
  PDst := PInt8u(PtrComp(This.RenderingBuffer.Row(Ydst)) + (Xdst shl 2) *
    SizeOf(Int8u));
  Incp := 4;

  if Xdst > Xsrc then
  begin
    inc(PtrComp(PSrc), (Len - 1) shl 2);
    inc(PtrComp(PDst), (Len - 1) shl 2);

    Incp := -4;
  end;

  repeat
    This.Blender(This, This.BlendMode, PDst,
      PInt8u(PtrComp(PSrc) + From.Order.r)^,
      PInt8u(PtrComp(PSrc) + From.Order.g)^,
      PInt8u(PtrComp(PSrc) + From.Order.b)^,
      PInt8u(PtrComp(PSrc) + From.Order.a)^, Cover);

    inc(PtrComp(PSrc), Incp);
    inc(PtrComp(PDst), Incp);
    dec(Len);
  until Len = 0;
end;

procedure CublCopyColorHSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2) * SizeOf(Int8u));

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

procedure CublCopyColorVSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2) * SizeOf(Int8u));

  repeat
    PInt8u(PtrComp(p) + This.Order.r)^ := Colors.Rgba8.r;
    PInt8u(PtrComp(p) + This.Order.g)^ := Colors.Rgba8.g;
    PInt8u(PtrComp(p) + This.Order.b)^ := Colors.Rgba8.b;
    PInt8u(PtrComp(p) + This.Order.a)^ := Colors.Rgba8.a;

    p := This.RenderingBuffer.NextRow(p);

    inc(PtrComp(Colors), SizeOf(TAggColor));
    dec(Len);
  until Len = 0;
end;

procedure CublBlendFromColor(This, From: TAggPixelFormatProcessor;
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
      (Xdst shl 2));

    repeat
      This.Blender(This, This.BlendMode, PDst, COLOR.Rgba8.r, COLOR.Rgba8.g,
        COLOR.Rgba8.b, COLOR.Rgba8.a, ShrInt32(PSrc^ * Cover + CAggBaseMask,
        CAggBaseShift));

      inc(PtrComp(PSrc), Ppsz);
      inc(PtrComp(PDst), 4);
      dec(Len);
    until Len = 0;
  end;
end;

procedure CublBlendFromLUT(This, From: TAggPixelFormatProcessor;
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
      (Xdst shl 2));

    repeat
      COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

      This.Blender(This, This.BlendMode, PDst, COLOR.Rgba8.r, COLOR.Rgba8.g,
        COLOR.Rgba8.b, COLOR.Rgba8.a, ShrInt32(PSrc^ * Cover + CAggBaseMask,
        CAggBaseShift));

      inc(PtrComp(PSrc), Ppsz);
      inc(PtrComp(PDst), 4);
      dec(Len);
    until Len = 0;
  end;
end; 
 
 
 
