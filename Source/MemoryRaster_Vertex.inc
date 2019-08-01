function EnsureCCWinding(var t: TTriangle): Boolean; inline;
begin
  Result := (t[1, 0] - t[0, 0]) * (t[2, 1] - t[0, 1]) > (t[2, 0] - t[0, 0]) * (t[1, 1] - t[0, 1]);
end;

function TexCoordWrapRepeat(const f: TGeoFloat): TGeoFloat;
begin
  if f < 0 then
      Result := 1 - Trunc(f) + f
  else if f > 1 then
      Result := f - Trunc(f)
  else
      Result := f;
end;

procedure Vertex_ComputeBlend(const Sender: PVertexMap; const f, M: TRColor; var b: TRColor);
begin
  if M >= $FF then
      BlendMem(f, b)
  else
      BlendMemEx(f, b, M);
end;

function Vertex_ComputeNearest(const Sender: PVertexMap; const Sampler: TMemoryRaster; const x, y: TGeoFloat): TRColor;
var
  tu0, tv0: Integer;
begin
  tu0 := ClampInt(Trunc(TexCoordWrapRepeat(x) * (Sampler.Width0)), 0, Sampler.Width0i);
  tv0 := ClampInt(Trunc(TexCoordWrapRepeat(y) * (Sampler.Height0)), 0, Sampler.Height0i);
  Result := Sampler.Pixel[tu0, tv0];
end;

function Vertex_ComputeLinear(const Sender: PVertexMap; const Sampler: TMemoryRaster; const x, y: TGeoFloat): TRColor;
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
  c1, c2, c3, c4: TRColorEntry;
  k1, k2, k3, K4: TGeoFloat;
  r, g, b, a: TGeoFloat;
begin
  fx := TexCoordWrapRepeat(x) * Sampler.Width0;
  fy := TexCoordWrapRepeat(y) * Sampler.Height0;

  i_x := ClampInt(Trunc(fx), 0, Sampler.Width0i);
  i_y := ClampInt(Trunc(fy), 0, Sampler.Height0i);

  i_x2 := i_x + 1;
  i_y2 := i_y + 1;
  if (i_x2 > Sampler.Width0) or (i_y2 > Sampler.Height0) then
    begin
      Result := Sampler[i_x, i_y];
      exit;
    end;

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

  TRColorEntry(Result).r := Trunc(r * $FF);
  TRColorEntry(Result).g := Trunc(g * $FF);
  TRColorEntry(Result).b := Trunc(b * $FF);
  TRColorEntry(Result).a := Trunc(a * $FF);
end;

procedure TVertexMap.RasterizeTriangle(const FS: TFragSampling; const sc: TRColor; const tex: TMemoryRaster; const SamplerTri, RenderTri: TTriangle);

  function ComputeDeterminant: TGeoFloat;
  var
    x1, x2, x3, y1, y2, y3: TGeoFloat;
  begin
    x1 := RenderTri[0, 0];
    y1 := RenderTri[0, 1];
    x2 := RenderTri[1, 0];
    y2 := RenderTri[1, 1];
    x3 := RenderTri[2, 0];
    y3 := RenderTri[2, 1];
    Result := x1 * y2 - x2 * y1 + x2 * y3 - x3 * y2 + x3 * y1 - x1 * y3;
  end;

  function ComputeInterpolationConsts(const Determinant, W1, W2, W3: TGeoFloat): TBilerpConsts;
  var
    x1, x2, x3, y1, y2, y3: TGeoFloat;
  begin
    x1 := RenderTri[0, 0];
    y1 := RenderTri[0, 1];
    x2 := RenderTri[1, 0];
    y2 := RenderTri[1, 1];
    x3 := RenderTri[2, 0];
    y3 := RenderTri[2, 1];
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
    x1 := Trunc(RenderTri[0, 0] * 16);
    y1 := Trunc(RenderTri[0, 1] * 16);
    x2 := Trunc(RenderTri[1, 0] * 16);
    y2 := Trunc(RenderTri[1, 1] * 16);
    x3 := Trunc(RenderTri[2, 0] * 16);
    y3 := Trunc(RenderTri[2, 1] * 16);

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

    minX := Max(-Window.Width, minX);
    minX := Min(Window.Width, minX);
    maxX := Min(Window.Width, maxX);
    maxX := Max(-Window.Width, maxX);

    minY := Max(-Window.Height, minY);
    minY := Min(Window.Height, minY);
    maxY := Min(Window.Height, maxY);
    maxY := Max(-Window.Height, maxY);

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
      exit;
  attr_u := ComputeInterpolationConsts(Determinant, SamplerTri[0, 0], SamplerTri[1, 0], SamplerTri[2, 0]);
  attr_v := ComputeInterpolationConsts(Determinant, SamplerTri[0, 1], SamplerTri[1, 1], SamplerTri[2, 1]);

  // Scan through bounding rectangle
  bitDst := minY * Window.Width;
  j := minY;
  while j < maxY do
    begin
      CX1 := CY1;
      CX2 := CY2;
      CX3 := CY3;
      row_frag_count := 0;
      row_last_x := 0;
      if (j >= 0) and (j < Window.Height) then // if empty line
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
      inc(bitDst, Window.Width);
      inc(j);
    end;
end;

procedure TVertexMap.FillFragment(const FS: TFragSampling; const sc: TRColor; const tex: TMemoryRaster;
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
      exit;
  old := FNearestWriterID;
  inc(FNearestWriterID);
  if FNearestWriterID < old then
    begin
      FillPtrByte(@FNearestWriteBuffer[0], length(FNearestWriteBuffer), FNearestWriterID);
      inc(FNearestWriterID);
    end;
end;

procedure TVertexMap.internal_Draw(const RenderTri: TTriangle; const Sampler: TRColor);
var
  nRenderTri: TTriangle;
begin
  nRenderTri := RenderTri;
  {
    Make sure the triangle has counter-clockwise winding
    For a triangle A B C, you can find the winding by computing the cross product (B - A) rx (C - A).
    For 2d tri's, with z=0, it will only have a z component.
    To give all the same winding, swap vertices C and B if this z component is negative.
  }
  if EnsureCCWinding(nRenderTri) then
      SwapVec2(nRenderTri[1], nRenderTri[2]);

  RasterizeTriangle(TFragSampling.fsSolid, Sampler, nil, ZeroTriangle, TriRound(TriMul(nRenderTri, Window.Size0)));

  if DrawTriangleEdge then
      Window.DrawTriangle(RenderTri, True, TriangleEdgeColor, False);
end;

procedure TVertexMap.internal_Draw(const SamplerTri, RenderTri: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean);
var
  nSamplerTri, nRenderTri: TTriangle;
  f: TFragSampling;
begin
  nSamplerTri := SamplerTri;
  nRenderTri := RenderTri;
  {
    Make sure the triangle has counter-clockwise winding
    For a triangle A B C, you can find the winding by computing the cross product (B - A) rx (C - A).
    For 2d tri's, with z=0, it will only have a z component.
    To give all the same winding, swap vertices C and B if this z component is negative.
  }
  if EnsureCCWinding(nRenderTri) then
    begin
      SwapVec2(nRenderTri[1], nRenderTri[2]);
      SwapVec2(nSamplerTri[1], nSamplerTri[2]);
    end;

  // offset texel centers
  nSamplerTri := TriSub(nSamplerTri, Vec2(1.0 / (2.0 * Sampler.Width0), 1.0 / (2.0 * Sampler.Height0)));
  // scale vertices to pixel grid
  nRenderTri := TriRound(TriMul(nRenderTri, Window.Size0));

  if bilinear_sampling then
      f := TFragSampling.fsLinear
  else
      f := TFragSampling.fsNearest;

  RasterizeTriangle(f, RColor(0, 0, 0), Sampler, nSamplerTri, nRenderTri);

  if DrawTriangleEdge then
      Window.DrawTriangle(RenderTri, True, TriangleEdgeColor, False);
end;

procedure TVertexMap.internal_Draw(const SamplerTri, RenderTri: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: Single);
var
  MA: Cardinal;
begin
  MA := Sampler.MasterAlpha;
  if alpha > 1.0 then
      Sampler.MasterAlpha := ClampByte(Trunc(alpha))
  else
      Sampler.MasterAlpha := ClampByte(Trunc(alpha * $FF));
  internal_Draw(SamplerTri, RenderTri, Sampler, bilinear_sampling);
  Sampler.MasterAlpha := MA;
end;

constructor TVertexMap.Create(raster: TMemoryRaster);
begin
  inherited Create;
  SetLength(FNearestWriteBuffer, raster.Width * raster.Height);
  FNearestWriterID := 0;
  FCurrentUpdate := 0;
  ComputeBlend := {$IFDEF FPC}@{$ENDIF FPC}Vertex_ComputeBlend;
  ComputeNearest := {$IFDEF FPC}@{$ENDIF FPC}Vertex_ComputeNearest;
  ComputeLinear := {$IFDEF FPC}@{$ENDIF FPC}Vertex_ComputeLinear;
  DrawTriangleEdge := False;
  TriangleEdgeColor := RColor($FF, $7F, $7F, $7F);
  Window := raster;
  WindowSize := Window.Width * Window.Height;
  UserData := nil;
end;

destructor TVertexMap.Destroy;
begin
  SetLength(FNearestWriteBuffer, 0);
  inherited Destroy;
end;

function TVertexMap.BeginUpdate: Byte;
begin
  if FCurrentUpdate = 0 then
      NewWriterBuffer;
  inc(FCurrentUpdate);
  Result := FNearestWriterID;
end;

procedure TVertexMap.EndUpdate;
begin
  dec(FCurrentUpdate);
end;

procedure TVertexMap.DrawTriangle(const v1, v2, v3: TVec2; const Sampler: TRColor);
begin
  NewWriterBuffer;
  internal_Draw(TriDiv(Tri(v1, v2, v3), Window.Size0), Sampler);
end;

procedure TVertexMap.DrawTriangle(const RenderTri: TTriangle; const Sampler: TRColor);
begin
  NewWriterBuffer;
  internal_Draw(TriDiv(RenderTri, Window.Size0), Sampler);
end;

procedure TVertexMap.DrawTriangle(const SamplerTri, RenderTri: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean);
begin
  NewWriterBuffer;
  internal_Draw(TriDiv(SamplerTri, Sampler.Size0), TriDiv(RenderTri, Window.Size0), Sampler, bilinear_sampling);
end;

procedure TVertexMap.DrawTriangle(const SamplerTri, RenderTri: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: TGeoFloat);
begin
  NewWriterBuffer;
  internal_Draw(TriDiv(SamplerTri, Sampler.Size0), TriDiv(RenderTri, Window.Size0), Sampler, bilinear_sampling, alpha);
end;

procedure TVertexMap.DrawRect(const RenVec: TV2Rect4; const Sampler: TRColor);
var
  RV: TV2Rect4;
  RenderTri: TTriangle;
begin
  NewWriterBuffer;
  RV := RenVec.FDiv(Window.Size0);
  RenderTri[0] := RV.LeftTop;
  RenderTri[1] := RV.RightTop;
  RenderTri[2] := RV.LeftBottom;
  internal_Draw(RenderTri, Sampler);
  RenderTri[0] := RV.LeftBottom;
  RenderTri[1] := RV.RightTop;
  RenderTri[2] := RV.RightBottom;
  internal_Draw(RenderTri, Sampler);
end;

procedure TVertexMap.DrawRect(const SamVec, RenVec: TV2Rect4; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: Single);
var
  SV, RV: TV2Rect4;
  SamplerTri, RenderTri: TTriangle;
begin
  NewWriterBuffer;
  SV := SamVec.FDiv(Sampler.Size0);
  RV := RenVec.FDiv(Window.Size0);

  SamplerTri[0] := SV.LeftTop;
  SamplerTri[1] := SV.RightTop;
  SamplerTri[2] := SV.LeftBottom;
  RenderTri[0] := RV.LeftTop;
  RenderTri[1] := RV.RightTop;
  RenderTri[2] := RV.LeftBottom;
  internal_Draw(SamplerTri, RenderTri, Sampler, bilinear_sampling, alpha);

  SamplerTri[0] := SV.LeftBottom;
  SamplerTri[1] := SV.RightTop;
  SamplerTri[2] := SV.RightBottom;
  RenderTri[0] := RV.LeftBottom;
  RenderTri[1] := RV.RightTop;
  RenderTri[2] := RV.RightBottom;
  internal_Draw(SamplerTri, RenderTri, Sampler, bilinear_sampling, alpha);
end;

procedure TVertexMap.DrawRect(const RenVec: TRectV2; const Sampler: TRColor);
begin
  DrawRect(TV2Rect4.Init(RenVec, 0), Sampler);
end;

procedure TVertexMap.DrawRect(const SamVec, RenVec: TRectV2; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: Single);
begin
  DrawRect(TV2Rect4.Init(SamVec, 0), TV2Rect4.Init(RenVec, 0), Sampler, bilinear_sampling, alpha);
end;

procedure TVertexMap.DrawRect(const RenVec: TRectV2; const RenAngle: TGeoFloat; const Sampler: TRColor);
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

procedure TVertexMap.FillPoly(const RenVec: TVec2List; const cen: TVec2; const Sampler: TRColor);
var
  RV: TVec2List;
  rCen: TVec2;
  i: Integer;
  RenderTri: TTriangle;
begin
  NewWriterBuffer;

  RV := TVec2List.Create;
  RV.Assign(RenVec);
  RV.FDiv(Window.Width0, Window.Height0);
  rCen := Vec2Div(cen, Window.Size0);

  for i := 1 to RV.Count - 1 do
    begin
      RenderTri[0] := rCen;
      RenderTri[1] := RV[i]^;
      RenderTri[2] := RV[i - 1]^;
      internal_Draw(RenderTri, Sampler);
    end;

  RenderTri[0] := rCen;
  RenderTri[1] := RV.First^;
  RenderTri[2] := RV.Last^;
  internal_Draw(RenderTri, Sampler);
  DisposeObject(RV);
end;

procedure TVertexMap.FillPoly(const RenVec: TVec2List; const Sampler: TRColor);
begin
  FillPoly(RenVec, RenVec.Centroid, Sampler);
end;

procedure TVertexMap.FillPoly(const SamVec, RenVec: TVec2List; const SamCen, RenCen: TVec2; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: Single);
var
  SV, RV: TVec2List;
  sCen, rCen: TVec2;
  i: Integer;
  SamplerTri, RenderTri: TTriangle;
begin
  if SamVec.Count <> RenVec.Count then
      RaiseInfo('vertex error');

  NewWriterBuffer;

  SV := TVec2List.Create;
  SV.Assign(SamVec);
  SV.FDiv(Sampler.Width0, Sampler.Height0);
  sCen := Vec2Div(SamCen, Sampler.Size0);

  RV := TVec2List.Create;
  RV.Assign(RenVec);
  RV.FDiv(Window.Width0, Window.Height0);
  rCen := Vec2Div(RenCen, Window.Size0);

  for i := 1 to SV.Count - 1 do
    begin

      SamplerTri[0] := sCen;
      SamplerTri[1] := SV[i]^;
      SamplerTri[2] := SV[i - 1]^;

      RenderTri[0] := rCen;
      RenderTri[1] := RV[i]^;
      RenderTri[2] := RV[i - 1]^;
      internal_Draw(SamplerTri, RenderTri, Sampler, bilinear_sampling, alpha);
    end;

  SamplerTri[0] := sCen;
  SamplerTri[1] := SV.First^;
  SamplerTri[2] := SV.Last^;

  RenderTri[0] := rCen;
  RenderTri[1] := RV.First^;
  RenderTri[2] := RV.Last^;
  internal_Draw(SamplerTri, RenderTri, Sampler, bilinear_sampling, alpha);

  DisposeObject(SV);
  DisposeObject(RV);
end;

procedure TVertexMap.FillPoly(const SamVec, RenVec: TVec2List; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: Single);
begin
  FillPoly(SamVec, RenVec, SamVec.Centroid, RenVec.Centroid, Sampler, bilinear_sampling, alpha);
end;
