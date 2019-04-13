function EnsureCcWinding(var t: TTriangle): Boolean;
  procedure swap_vertex(var a, b: TVert2);
  var
    v: TVert2;
  begin
    v := a;
    a := b;
    b := v;
  end;

{
  Make sure the triangle has counter-clockwise winding
  For a triangle A B C, you can find the winding by computing the cross product (B - A) rx (C - A).
  For 2d tri's, with z=0, it will only have a z component.
  To give all the same winding, swap vertices C and B if this z component is negative.
}
begin
  Result := False;
  if (t[1].Render[0] - t[0].Render[0]) * (t[2].Render[1] - t[0].Render[1]) > (t[2].Render[0] - t[0].Render[0]) * (t[1].Render[1] - t[0].Render[1]) then
    begin
      swap_vertex(t[1], t[2]);
      Result := True;
    end;
end;

function TexCoordWrapRepeat(const f: TGeoFloat): TGeoFloat;
{ emulate OpenGL4.4 GL_TEXTURE_WRAP_S/T: GL_REPEAT }
begin
  if f < 0 then
      Result := 1 - Trunc(f) + f
  else if f > 1 then
      Result := f - Trunc(f)
  else
      Result := f;
end;

procedure Vertex_ComputeBlend(const Sender: PVertexMap; const f, M: TRasterColor; var b: TRasterColor);
begin
  if M >= 255 then
      BlendMem(f, b)
  else
      BlendMemEx(f, b, M);
end;

function Vertex_ComputeNearest(const Sender: PVertexMap; const Sampler: TMemoryRaster; const x, y: TGeoFloat): TRasterColor;
var
  tu0, tv0: Integer;
begin
  tu0 := ClampInt_(Trunc(TexCoordWrapRepeat(x) * (Sampler.width)), 0, Sampler.width - 1);
  tv0 := ClampInt_(Trunc(TexCoordWrapRepeat(y) * (Sampler.height)), 0, Sampler.height - 1);
  Result := Sampler.Pixel[tu0, tv0];
end;

function Vertex_ComputeLinear(const Sender: PVertexMap; const Sampler: TMemoryRaster; const x, y: TGeoFloat): TRasterColor;
  procedure ClampF(var f: TGeoFloat); inline;
  begin
    if f < 0 then
        f := 0
    else if f > 1 then
        f := 1;
  end;

var
  fx, fy: TGeoFloat;
  i_x, i_y: Integer;
  i_x2, i_y2: Integer;
  delta_x, delta_y: TGeoFloat;
  c1, c2, c3, c4: TRasterColorEntry;
  k1, k2, k3, K4: TGeoFloat;
  r, g, b, a: TGeoFloat;
begin
  fx := TexCoordWrapRepeat(x) * Sampler.width;
  fy := TexCoordWrapRepeat(y) * Sampler.height;

  i_x := ClampInt_(Trunc(fx), 0, Sampler.width - 1);
  i_y := ClampInt_(Trunc(fy), 0, Sampler.height - 1);

  i_x2 := i_x + 1;
  if i_x2 > Sampler.width - 1 then
      i_x2 := 0;
  i_y2 := i_y + 1;
  if i_y2 > Sampler.height - 1 then
      i_y2 := 0;

  delta_x := Frac(fx);
  delta_y := Frac(fy);

  k1 := (1 - delta_x) * (1 - delta_y);
  k2 := delta_x * (1 - delta_y);
  k3 := delta_x * delta_y;
  K4 := (1 - delta_x) * delta_y;

  c1.BGRA := Sampler[i_x, i_y];
  c2.BGRA := Sampler[i_x2, i_y];
  c3.BGRA := Sampler[i_x2, i_y2];
  c4.BGRA := Sampler[i_x, i_y2];

  r := ((c1.r / $FF) * k1) + ((c2.r / $FF) * k2) + ((c3.r / $FF) * k3) + ((c4.r / $FF) * K4);
  g := ((c1.g / $FF) * k1) + ((c2.g / $FF) * k2) + ((c3.g / $FF) * k3) + ((c4.g / $FF) * K4);
  b := ((c1.b / $FF) * k1) + ((c2.b / $FF) * k2) + ((c3.b / $FF) * k3) + ((c4.b / $FF) * K4);
  a := ((c1.a / $FF) * k1) + ((c2.a / $FF) * k2) + ((c3.a / $FF) * k3) + ((c4.a / $FF) * K4);

  ClampF(r);
  ClampF(g);
  ClampF(b);
  ClampF(a);

  TRasterColorEntry(Result).r := Trunc(r * 255);
  TRasterColorEntry(Result).g := Trunc(g * 255);
  TRasterColorEntry(Result).b := Trunc(b * 255);
  TRasterColorEntry(Result).a := Trunc(a * 255);
end;

procedure TVertexMap.RasterizeTriangle(const FS: TFragSampling; const sc: TRasterColor; const tex: TMemoryRaster; const t: TTriangle);

  function ComputeDeterminant: TGeoFloat;
  var
    x1, x2, x3, y1, y2, y3: TGeoFloat;
  begin
    x1 := t[0].Render[0];
    y1 := t[0].Render[1];
    x2 := t[1].Render[0];
    y2 := t[1].Render[1];
    x3 := t[2].Render[0];
    y3 := t[2].Render[1];
    Result := x1 * y2 - x2 * y1 + x2 * y3 - x3 * y2 + x3 * y1 - x1 * y3;
  end;

  function ComputeInterpolationConsts(const Determinant, W1, W2, W3: TGeoFloat): TBilerpConsts;
  var
    x1, x2, x3, y1, y2, y3: TGeoFloat;
  begin
    x1 := t[0].Render[0];
    y1 := t[0].Render[1];
    x2 := t[1].Render[0];
    y2 := t[1].Render[1];
    x3 := t[2].Render[0];
    y3 := t[2].Render[1];
    Result.a := ((y2 - y3) * W1 + (y3 - y1) * W2 + (y1 - y2) * W3) / Determinant;
    Result.b := ((x3 - x2) * W1 + (x1 - x3) * W2 + (x2 - x1) * W3) / Determinant;
    Result.c := ((x2 * y3 - x3 * y2) * W1 + (x3 * y1 - x1 * y3) * W2 + (x1 * y2 - x2 * y1) * W3) / Determinant;
  end;

var
  DX12, DX23, DX31, DY12, DY23, DY31: Integer;
  CY1, CY2, CY3: Integer;
  CX1, CX2, CX3: Integer;
  minX, maxX, minY, maxY: Integer;
  i, j: Integer;
  row_last_x: Integer;
  row_frag_count: Integer;
  bitDst: Integer;
  Determinant: TGeoFloat;
  attr_u, attr_v: TBilerpConsts;

  procedure RasterizationSetup;
    function min3(const a, b, c: Integer): Integer;
    begin
      Result := Min(a, Min(b, c));
      // seealso result mod 16 > 0
      if Result > 0 then
          Result := (Result + 15) div 16
      else
          Result := (Result - 15) div 16;
    end;

    function max3(const a, b, c: Integer): Integer;
    begin
      Result := Max(a, Max(b, c));
      // seealso result mod 16 > 0
      if Result > 0 then
          Result := (Result + 15) div 16
      else
          Result := (Result - 15) div 16;
    end;

  var
    x1, x2, x3, y1, y2, y3, c1, c2, c3: Integer;
  begin
    // fixed-point coordinates
    x1 := Trunc(t[0].Render[0] * 16);
    y1 := Trunc(t[0].Render[1] * 16);
    x2 := Trunc(t[1].Render[0] * 16);
    y2 := Trunc(t[1].Render[1] * 16);
    x3 := Trunc(t[2].Render[0] * 16);
    y3 := Trunc(t[2].Render[1] * 16);

    // Deltas
    DX12 := x1 - x2;
    DX23 := x2 - x3;
    DX31 := x3 - x1;
    DY12 := y1 - y2;
    DY23 := y2 - y3;
    DY31 := y3 - y1;

    // Bounding rectangle
    minX := min3(x1, x2, x3);
    maxX := max3(x1, x2, x3);
    minY := min3(y1, y2, y3);
    maxY := max3(y1, y2, y3);

    minX := Max(-Window.width, minX);
    minX := Min(Window.width, minX);
    maxX := Min(Window.width, maxX);
    maxX := Max(-Window.width, maxX);

    minY := Max(-Window.height, minY);
    minY := Min(Window.height, minY);
    maxY := Min(Window.height, maxY);
    maxY := Max(-Window.height, maxY);

    if minX > maxX then
        Swap(minX, maxX);
    if minY > maxY then
        Swap(minY, maxY);

    // Half-edge constants
    // see also,sar(x, 4)
    c1 := (DY12 * x1 - DX12 * y1) div 16;
    c2 := (DY23 * x2 - DX23 * y2) div 16;
    c3 := (DY31 * x3 - DX31 * y3) div 16;

    // Correct for fill convention
    if (DY12 < 0) or ((DY12 = 0) and (DX12 > 0)) then
        inc(c1);
    if (DY23 < 0) or ((DY23 = 0) and (DX23 > 0)) then
        inc(c2);
    if (DY31 < 0) or ((DY31 = 0) and (DX31 > 0)) then
        inc(c3);

    CY1 := c1 + DX12 * minY - DY12 * minX;
    CY2 := c2 + DX23 * minY - DY23 * minX;
    CY3 := c3 + DX31 * minY - DY31 * minX;
  end;

begin
  // pixel rasterization
  RasterizationSetup;

  // init triangle interpolation
  Determinant := ComputeDeterminant;
  if Determinant = 0 then
      Exit;
  attr_u := ComputeInterpolationConsts(Determinant, t[0].Sampler[0], t[1].Sampler[0], t[2].Sampler[0]);
  attr_v := ComputeInterpolationConsts(Determinant, t[0].Sampler[1], t[1].Sampler[1], t[2].Sampler[1]);

  // Scan through bounding rectangle
  bitDst := minY * Window.width;
  j := minY;
  while j < maxY do
    begin
      CX1 := CY1;
      CX2 := CY2;
      CX3 := CY3;
      row_frag_count := 0;
      row_last_x := 0;
      if (j >= 0) and (j < Window.height) then // if empty line
        begin
          i := minX;
          while i < maxX do
            begin
              // When all half-space functions positive, pixel is in triangle
              // fast test only sign bits
              if (CX1 or CX2 or CX3) > 0 then
                begin
                  inc(row_frag_count);
                  row_last_x := i;
                end;
              dec(CX1, DY12);
              dec(CX2, DY23);
              dec(CX3, DY31);
              inc(i);
            end;
          if (row_frag_count > 0) then
              FillFragment(FS, sc, tex, bitDst, j, row_last_x + 1 - row_frag_count, row_frag_count, attr_v, attr_u);
        end;

      inc(CY1, DX12);
      inc(CY2, DX23);
      inc(CY3, DX31);
      inc(bitDst, Window.width);
      inc(j);
    end;
end;

procedure TVertexMap.FillFragment(const FS: TFragSampling; const sc: TRasterColor; const tex: TMemoryRaster;
  const bitDst, j, start_x, frag_count: Integer; const attr_v, attr_u: TBilerpConsts);
var
  i, bitX: Integer;
  x, y: TGeoFloat;
begin
  Window.ReadyBits();
  x := attr_u.a * start_x + attr_u.b * j + attr_u.c;
  y := attr_v.a * start_x + attr_v.b * j + attr_v.c;

  bitX := bitDst + start_x;
  i := 0;
  while i < frag_count do
    begin
      if (bitX >= 0) and (bitX < WindowSize) and (FNearestWriteBuffer[bitX] <> FNearestWriterID) then
        begin
          case FS of
            fsSolid: BlendMem(sc, Window.FBits^[bitX]);
            fsNearest: ComputeBlend(@Self, ComputeNearest(@Self, tex, x, y), tex.MasterAlpha, Window.FBits^[bitX]);
            fsLinear: ComputeBlend(@Self, ComputeLinear(@Self, tex, x, y), tex.MasterAlpha, Window.FBits^[bitX]);
          end;
          FNearestWriteBuffer[bitX] := FNearestWriterID;
        end;

      x := x + attr_u.a;
      y := y + attr_v.a;
      inc(bitX);
      inc(i);
    end;
end;

procedure TVertexMap.NewWriterBuffer;
var
  old: Byte;
begin
  if FCurrentUpdate > 0 then
      Exit;
  old := FNearestWriterID;
  inc(FNearestWriterID);
  if FNearestWriterID < old then
    begin
      FillPtrByte(@FNearestWriteBuffer[0], length(FNearestWriteBuffer), FNearestWriterID);
      inc(FNearestWriterID);
    end;
end;

procedure TVertexMap.internal_Draw(const Triangle: TTriangle; const Sampler: TRasterColor);
var
  nt: TTriangle;
  i: Integer;
begin
  nt := Triangle;
  // triangle must have counter-clockwise winding
  EnsureCcWinding(nt);

  // scale vertices to pixel grid
  nt[0].Render[0] := Round(nt[0].Render[0] * Window.width);
  nt[0].Render[1] := Round(nt[0].Render[1] * Window.height);
  nt[1].Render[0] := Round(nt[1].Render[0] * Window.width);
  nt[1].Render[1] := Round(nt[1].Render[1] * Window.height);
  nt[2].Render[0] := Round(nt[2].Render[0] * Window.width);
  nt[2].Render[1] := Round(nt[2].Render[1] * Window.height);

  RasterizeTriangle(TFragSampling.fsSolid, Sampler, nil, nt);
end;

procedure TVertexMap.internal_Draw(const Triangle: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean);
var
  nt: TTriangle;
  offset_u, offset_v: TGeoFloat;
  i: Integer;
begin
  nt := Triangle;
  // triangle must have counter-clockwise winding
  EnsureCcWinding(nt);

  // scale vertices to pixel grid
  nt[0].Render[0] := Round(nt[0].Render[0] * Window.width);
  nt[0].Render[1] := Round(nt[0].Render[1] * Window.height);
  nt[1].Render[0] := Round(nt[1].Render[0] * Window.width);
  nt[1].Render[1] := Round(nt[1].Render[1] * Window.height);
  nt[2].Render[0] := Round(nt[2].Render[0] * Window.width);
  nt[2].Render[1] := Round(nt[2].Render[1] * Window.height);

  // offset texel centers
  offset_u := 1 / (2 * Sampler.width);
  offset_v := 1 / (2 * Sampler.height);
  for i := 0 to 2 do
    begin
      nt[i].Sampler[0] := nt[i].Sampler[0] - offset_u;
      nt[i].Sampler[1] := nt[i].Sampler[1] - offset_v;
    end;

  if bilinear_sampling then
      RasterizeTriangle(TFragSampling.fsLinear, RasterColor(0, 0, 0), Sampler, nt)
  else
      RasterizeTriangle(TFragSampling.fsNearest, RasterColor(0, 0, 0), Sampler, nt);
end;

procedure TVertexMap.internal_Draw(const Triangle: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: Single);
var
  MA: Cardinal;
begin
  MA := Sampler.MasterAlpha;
  if alpha > 1.0 then
      Sampler.MasterAlpha := ClampByte_(Trunc(alpha))
  else
      Sampler.MasterAlpha := ClampByte_(Trunc(alpha * 255));
  internal_Draw(Triangle, Sampler, bilinear_sampling);
  Sampler.MasterAlpha := MA;
end;

constructor TVertexMap.Create(raster: TMemoryRaster);
begin
  inherited Create;
  SetLength(FNearestWriteBuffer, raster.width * raster.height);
  FNearestWriterID := 0;
  FCurrentUpdate := 0;
  ComputeBlend := {$IFDEF FPC}@{$ENDIF FPC}Vertex_ComputeBlend;
  ComputeNearest := {$IFDEF FPC}@{$ENDIF FPC}Vertex_ComputeNearest;
  ComputeLinear := {$IFDEF FPC}@{$ENDIF FPC}Vertex_ComputeLinear;
  Window := raster;
  WindowSize := Window.width * Window.height;
  UserData := nil;
end;

destructor TVertexMap.Destroy;
begin
  SetLength(FNearestWriteBuffer, 0);
  inherited Destroy;
end;

procedure TVertexMap.BeginUpdate;
begin
  if FCurrentUpdate = 0 then
      NewWriterBuffer;
  inc(FCurrentUpdate);
end;

procedure TVertexMap.EndUpdate;
begin
  dec(FCurrentUpdate);
end;

procedure TVertexMap.DrawTriangle(const t1, t2, t3: TVec2; const Sampler: TRasterColor);
var
  t: TTriangle;
begin
  NewWriterBuffer;

  t[0].Sampler := ZeroVec2;
  t[1].Sampler := ZeroVec2;
  t[2].Sampler := ZeroVec2;
  t[0].Render := Vec2Div(t1, Window.Size2D);
  t[1].Render := Vec2Div(t2, Window.Size2D);
  t[2].Render := Vec2Div(t3, Window.Size2D);
  internal_Draw(t, Sampler);
end;

procedure TVertexMap.DrawTriangle(const Triangle: TTriangle; const Sampler: TRasterColor);
var
  t: TTriangle;
begin
  NewWriterBuffer;

  t[0].Sampler := ZeroVec2;
  t[1].Sampler := ZeroVec2;
  t[2].Sampler := ZeroVec2;
  t[0].Render := Vec2Div(t[0].Render, Window.Size2D);
  t[1].Render := Vec2Div(t[1].Render, Window.Size2D);
  t[2].Render := Vec2Div(t[2].Render, Window.Size2D);
  internal_Draw(t, Sampler);
end;

procedure TVertexMap.DrawTriangle(const Triangle: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean);
var
  t: TTriangle;
begin
  NewWriterBuffer;

  t[0].Sampler := Vec2Div(t[0].Sampler, Sampler.Size2D);
  t[1].Sampler := Vec2Div(t[1].Sampler, Sampler.Size2D);
  t[2].Sampler := Vec2Div(t[2].Sampler, Sampler.Size2D);
  t[0].Render := Vec2Div(t[0].Render, Window.Size2D);
  t[1].Render := Vec2Div(t[1].Render, Window.Size2D);
  t[2].Render := Vec2Div(t[2].Render, Window.Size2D);
  internal_Draw(t, Sampler, bilinear_sampling);
end;

procedure TVertexMap.DrawTriangle(const Triangle: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: TGeoFloat);
var
  t: TTriangle;
begin
  NewWriterBuffer;

  t[0].Sampler := Vec2Div(t[0].Sampler, Sampler.Size2D);
  t[1].Sampler := Vec2Div(t[1].Sampler, Sampler.Size2D);
  t[2].Sampler := Vec2Div(t[2].Sampler, Sampler.Size2D);
  t[0].Render := Vec2Div(t[0].Render, Window.Size2D);
  t[1].Render := Vec2Div(t[1].Render, Window.Size2D);
  t[2].Render := Vec2Div(t[2].Render, Window.Size2D);
  internal_Draw(Triangle, Sampler, bilinear_sampling, alpha);
end;

procedure TVertexMap.DrawRect(const RenVec: TV2Rect4; const Sampler: TRasterColor);
var
  RV: TV2Rect4;
  t: TTriangle;
begin
  NewWriterBuffer;
  RV := RenVec.FDiv(vec2(Window.width, Window.height));
  t[0].Sampler := ZeroVec2;
  t[1].Sampler := ZeroVec2;
  t[2].Sampler := ZeroVec2;
  t[0].Render := RV.LeftTop;
  t[1].Render := RV.RightTop;
  t[2].Render := RV.LeftBottom;
  internal_Draw(t, Sampler);
  t[0].Render := RV.LeftBottom;
  t[1].Render := RV.RightTop;
  t[2].Render := RV.RightBottom;
  internal_Draw(t, Sampler);
end;

procedure TVertexMap.DrawRect(const SamVec, RenVec: TV2Rect4; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: Single);
var
  SV, RV: TV2Rect4;
  t: TTriangle;
begin
  NewWriterBuffer;
  SV := SamVec.FDiv(vec2(Sampler.width, Sampler.height));
  RV := RenVec.FDiv(vec2(Window.width, Window.height));
  t[0].Sampler := SV.LeftTop;
  t[0].Render := RV.LeftTop;
  t[1].Sampler := SV.RightTop;
  t[1].Render := RV.RightTop;
  t[2].Sampler := SV.LeftBottom;
  t[2].Render := RV.LeftBottom;
  internal_Draw(t, Sampler, bilinear_sampling, alpha);
  t[0].Sampler := SV.LeftBottom;
  t[0].Render := RV.LeftBottom;
  t[1].Sampler := SV.RightTop;
  t[1].Render := RV.RightTop;
  t[2].Sampler := SV.RightBottom;
  t[2].Render := RV.RightBottom;
  internal_Draw(t, Sampler, bilinear_sampling, alpha);
end;

procedure TVertexMap.DrawRect(const RenVec: TRectV2; const Sampler: TRasterColor);
begin
  DrawRect(TV2Rect4.Init(RenVec, 0), Sampler);
end;

procedure TVertexMap.DrawRect(const SamVec, RenVec: TRectV2; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: Single);
begin
  DrawRect(TV2Rect4.Init(SamVec, 0), TV2Rect4.Init(RenVec, 0), Sampler, bilinear_sampling, alpha);
end;

procedure TVertexMap.DrawRect(const RenVec: TRectV2; const RenAngle: TGeoFloat; const Sampler: TRasterColor);
begin
  DrawRect(TV2Rect4.Init(RenVec, RenAngle), Sampler);
end;

procedure TVertexMap.DrawRect(const SamVec, RenVec: TRectV2; const RenAngle: TGeoFloat; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: Single);
begin
  DrawRect(TV2Rect4.Init(SamVec, 0), TV2Rect4.Init(RenVec, RenAngle), Sampler, bilinear_sampling, alpha);
end;

procedure TVertexMap.DrawRect(const SamVec: TV2Rect4; const RenVec: TRectV2; const RenAngle: TGeoFloat; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: Single);
begin
  DrawRect(SamVec, TV2Rect4.Init(RenVec, RenAngle), Sampler, bilinear_sampling, alpha);
end;

procedure TVertexMap.FillPoly(const RenVec: TVec2List; const cen: TVec2; const Sampler: TRasterColor);
var
  RV: TVec2List;
  rCen: TVec2;
  i: Integer;
  t: TTriangle;
begin
  NewWriterBuffer;

  RV := TVec2List.Create;
  RV.Assign(RenVec);
  RV.FDiv(Window.width, Window.height);
  rCen := Vec2Div(cen, vec2(Window.width, Window.height));

  t[0].Sampler := ZeroVec2;
  t[1].Sampler := ZeroVec2;
  t[2].Sampler := ZeroVec2;

  for i := 1 to RV.Count - 1 do
    begin
      t[0].Render := rCen;
      t[1].Render := RV[i]^;
      t[2].Render := RV[i - 1]^;
      internal_Draw(t, Sampler);
    end;

  t[0].Render := rCen;
  t[1].Render := RV.First^;
  t[2].Render := RV.Last^;
  internal_Draw(t, Sampler);
  DisposeObject(RV);
end;

procedure TVertexMap.FillPoly(const RenVec: TVec2List; const Sampler: TRasterColor);
begin
  FillPoly(RenVec, RenVec.Centroid, Sampler);
end;

procedure TVertexMap.FillPoly(const SamVec, RenVec: TVec2List; const SamCen, RenCen: TVec2; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: Single);
var
  SV, RV: TVec2List;
  sCen, rCen: TVec2;
  i: Integer;
  t: TTriangle;
begin
  if SamVec.Count <> RenVec.Count then
      RaiseInfo('vertex error');

  NewWriterBuffer;

  SV := TVec2List.Create;
  SV.Assign(SamVec);
  SV.FDiv(Sampler.width, Sampler.height);
  sCen := Vec2Div(SamCen, vec2(Sampler.width, Sampler.height));

  RV := TVec2List.Create;
  RV.Assign(RenVec);
  RV.FDiv(Window.width, Window.height);
  rCen := Vec2Div(RenCen, vec2(Window.width, Window.height));

  for i := 1 to SV.Count - 1 do
    begin
      t[0].Sampler := sCen;
      t[0].Render := rCen;
      t[1].Sampler := SV[i]^;
      t[1].Render := RV[i]^;
      t[2].Sampler := SV[i - 1]^;
      t[2].Render := RV[i - 1]^;
      internal_Draw(t, Sampler, bilinear_sampling, alpha);
    end;

  t[0].Sampler := sCen;
  t[0].Render := rCen;
  t[1].Sampler := SV.First^;
  t[1].Render := RV.First^;
  t[2].Sampler := SV.Last^;
  t[2].Render := RV.Last^;
  internal_Draw(t, Sampler, bilinear_sampling, alpha);

  DisposeObject(SV);
  DisposeObject(RV);
end;

procedure TVertexMap.FillPoly(const SamVec, RenVec: TVec2List; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: Single);
begin
  FillPoly(SamVec, RenVec, SamVec.Centroid, RenVec.Centroid, Sampler, bilinear_sampling, alpha);
end;
