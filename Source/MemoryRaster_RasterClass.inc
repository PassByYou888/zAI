procedure MakeMergeTables;
var
  i, j: Integer;
const
  OneByteth: Double = 1.0 / 255.0;
begin
  for j := 0 to 255 do
    for i := 0 to 255 do
      begin
        DivTable[i, j] := Byte(Round(i * j * OneByteth));
        if i > 0 then
            RcTable[i, j] := Byte(Round(j * 255 / i))
        else
            RcTable[i, j] := 0;
      end;
end;

function TMemoryRaster.GetVertex: TVertexMap;
begin
  if FVertex = nil then
      OpenVertex;

  Result := FVertex;
end;

function TMemoryRaster.GetFont: TFontRaster;
begin
  if FFont = nil then
      OpenFont;

  Result := FFont;
end;

procedure TMemoryRaster.SetFont(f: TFontRaster);
begin
  CloseFont;
  FFont := TFontRaster.Create(f);
end;

function TMemoryRaster.GetAggImage: TMemoryRaster_AggImage;
begin
  if FAggImage = nil then
      OpenAgg;
  Result := FAggImage;
end;

function TMemoryRaster.GetAgg: TMemoryRaster_Agg2D;
begin
  if FAgg = nil then
      OpenAgg;
  Result := FAgg;
end;

procedure TMemoryRaster.FreeAgg;
begin
  if Assigned(FAggImage) then
    begin
      DisposeObject(FAggImage);
      FAggImage := nil;
    end;

  if Assigned(FAgg) then
    begin
      DisposeObject(FAgg);
      FAgg := nil;
    end;
end;

function TMemoryRaster.GetPixel(const x, y: Integer): TRasterColor;
begin
  Result := PixelPtr[x, y]^;
end;

procedure TMemoryRaster.SetPixel(const x, y: Integer; const Value: TRasterColor);
begin
  PixelPtr[x, y]^ := Value;
end;

function TMemoryRaster.GetPixelBGRA(const x, y: Integer): TRasterColor;
begin
  Result := RGBA2BGRA(GetPixel(x, y));
end;

procedure TMemoryRaster.SetPixelBGRA(const x, y: Integer; const Value: TRasterColor);
begin
  SetPixel(x, y, BGRA2RGBA(Value));
end;

function TMemoryRaster.GetPixelPtr(const x, y: Integer): PRasterColor;
begin
  Result := @(FBits^[ClampInt_(x, 0, width - 1) + ClampInt_(y, 0, height - 1) * width]);
end;

function TMemoryRaster.GetScanLine(y: Integer): PRasterColorArray;
begin
  Result := @(FBits^[y * FWidth]);
end;

function TMemoryRaster.GetPixelRed(const x, y: Integer): Byte;
begin
  Result := PRasterColorEntry(GetPixelPtr(x, y))^.r;
end;

procedure TMemoryRaster.SetPixelRed(const x, y: Integer; const Value: Byte);
begin
  PRasterColorEntry(GetPixelPtr(x, y))^.r := Value;
end;

function TMemoryRaster.GetPixelGreen(const x, y: Integer): Byte;
begin
  Result := PRasterColorEntry(GetPixelPtr(x, y))^.g;
end;

procedure TMemoryRaster.SetPixelGreen(const x, y: Integer; const Value: Byte);
begin
  PRasterColorEntry(GetPixelPtr(x, y))^.g := Value;
end;

function TMemoryRaster.GetPixelBlue(const x, y: Integer): Byte;
begin
  Result := PRasterColorEntry(GetPixelPtr(x, y))^.b;
end;

procedure TMemoryRaster.SetPixelBlue(const x, y: Integer; const Value: Byte);
begin
  PRasterColorEntry(GetPixelPtr(x, y))^.b := Value;
end;

function TMemoryRaster.GetPixelAlpha(const x, y: Integer): Byte;
begin
  Result := PRasterColorEntry(GetPixelPtr(x, y))^.a;
end;

procedure TMemoryRaster.SetPixelAlpha(const x, y: Integer; const Value: Byte);
begin
  PRasterColorEntry(GetPixelPtr(x, y))^.a := Value;
end;

function TMemoryRaster.GetGray(const x, y: Integer): Byte;
begin
  Result := RasterColor2Gray(GetPixel(x, y));
end;

procedure TMemoryRaster.SetGray(const x, y: Integer; const Value: Byte);
begin
  SetPixel(x, y, RasterColor(Value, Value, Value, 255));
end;

function TMemoryRaster.GetGrayS(const x, y: Integer): Single;
begin
  Result := RasterColor2GrayS(GetPixel(x, y));
end;

procedure TMemoryRaster.SetGrayS(const x, y: Integer; const Value: Single);
begin
  SetGray(x, y, ClampByte3_(Round(Value * $FF), 0, $FF));
end;

function TMemoryRaster.GetGrayD(const x, y: Integer): Double;
begin
  Result := RasterColor2GrayD(GetPixel(x, y));
end;

procedure TMemoryRaster.SetGrayD(const x, y: Integer; const Value: Double);
begin
  SetGrayS(x, y, Value);
end;

function TMemoryRaster.GetPixelF(const x, y: TGeoFloat): TRasterColor;
begin
  Result := GetPixel(Round(x), Round(y));
end;

procedure TMemoryRaster.SetPixelF(const x, y: TGeoFloat; const Value: TRasterColor);
begin
  SetPixel(Round(x), Round(y), Value);
end;

function TMemoryRaster.GetPixelVec(const v2: TVec2): TRasterColor;
begin
  Result := GetPixelF(v2[0], v2[1]);
end;

procedure TMemoryRaster.SetPixelVec(const v2: TVec2; const Value: TRasterColor);
begin
  SetPixelF(v2[0], v2[1], Value)
end;

function TMemoryRaster.GetPixelWrapLinear(const x, y: TGeoFloat): TRasterColor;
  function CoordWrap(const f: TGeoFloat): TGeoFloat; inline;
  begin
    if f < 0 then
        Result := 1 - Trunc(f) + f
    else if f > 1 then
        Result := f - Trunc(f)
    else
        Result := f;
  end;
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
  fx := CoordWrap(x) * width;
  fy := CoordWrap(y) * height;

  i_x := ClampInt_(Trunc(fx), 0, width - 1);
  i_y := ClampInt_(Trunc(fy), 0, height - 1);

  i_x2 := i_x + 1;
  if i_x2 > width - 1 then
      i_x2 := 0;
  i_y2 := i_y + 1;
  if i_y2 > height - 1 then
      i_y2 := 0;

  delta_x := Frac(fx);
  delta_y := Frac(fy);

  k1 := (1 - delta_x) * (1 - delta_y);
  k2 := delta_x * (1 - delta_y);
  k3 := delta_x * delta_y;
  K4 := (1 - delta_x) * delta_y;

  c1.BGRA := Pixel[i_x, i_y];
  c2.BGRA := Pixel[i_x2, i_y];
  c3.BGRA := Pixel[i_x2, i_y2];
  c4.BGRA := Pixel[i_x, i_y2];

  r := ((c1.r / $FF) * k1) + ((c2.r / $FF) * k2) + ((c3.r / $FF) * k3) + ((c4.r / $FF) * K4);
  g := ((c1.g / $FF) * k1) + ((c2.g / $FF) * k2) + ((c3.g / $FF) * k3) + ((c4.g / $FF) * K4);
  b := ((c1.b / $FF) * k1) + ((c2.b / $FF) * k2) + ((c3.b / $FF) * k3) + ((c4.b / $FF) * K4);
  a := ((c1.a / $FF) * k1) + ((c2.a / $FF) * k2) + ((c3.a / $FF) * k3) + ((c4.a / $FF) * K4);

  ClampF(r);
  ClampF(g);
  ClampF(b);
  ClampF(a);

  TRasterColorEntry(Result).r := Round(r * 255);
  TRasterColorEntry(Result).g := Round(g * 255);
  TRasterColorEntry(Result).b := Round(b * 255);
  TRasterColorEntry(Result).a := Round(a * 255);
end;

function TMemoryRaster.GetPixelLinear(const x, y: Integer): TRasterColor;
begin
  Result := GetPixelWrapLinear(x / width - 1, y / height - 1);
end;

constructor TMemoryRaster.Create;
begin
  inherited Create;
  FFreeBits := False;
  FBits := nil;
  FWidth := 0;
  FHeight := 0;
  FOuterColor := $00000000; // by default as full transparency black

  FMasterAlpha := $FF;
  FDrawMode := dmBlend;
  FCombineMode := cmBlend;

  FVertex := nil;
  FFont := nil;

  FAggImage := nil;
  FAgg := nil;
  FAggNeed := False;

  FUserObject := nil;
  FUserData := nil;
  FUserText := '';
  FUserToken := '';
  FUserVariant := NULL;
end;

destructor TMemoryRaster.Destroy;
begin
  CloseFont;
  CloseVertex;
  CloseAgg;
  if Assigned(FBits) and FFreeBits then
      FreeMem(FBits);
  inherited Destroy;
end;

procedure TMemoryRaster.SetWorkMemory(WorkMemory: Pointer; NewWidth, NewHeight: Integer);
begin
  CloseVertex;
  FreeAgg;

  if Assigned(FBits) and FFreeBits then
      FreeMem(FBits);

  if WorkMemory = nil then
    begin
      FFreeBits := True;
      FBits := nil;
      FWidth := NewWidth;
      FHeight := NewHeight;
    end
  else
    begin
      FFreeBits := False;
      FBits := PRasterColorArray(WorkMemory);
      FWidth := NewWidth;
      FHeight := NewHeight;
    end;

  if FAggNeed then
      OpenAgg;
end;

procedure TMemoryRaster.SetWorkMemory(raster: TMemoryRaster);
begin
  SetWorkMemory(@raster.FBits^[0], raster.width, raster.height);
end;

function TMemoryRaster.IsMemoryMap: Boolean;
begin
  Result := (FBits <> nil) and (not FFreeBits);
end;

procedure TMemoryRaster.OpenVertex;
begin
  if FVertex = nil then
      FVertex := TVertexMap.Create(Self);
end;

procedure TMemoryRaster.CloseVertex;
begin
  if FVertex <> nil then
    begin
      DisposeObject(FVertex);
      FVertex := nil;
    end;
end;

procedure TMemoryRaster.OpenFont;
begin
  Wait_SystemFont_Init;

  if FFont = nil then
      FFont := TFontRaster.Create(SystemFont);
end;

procedure TMemoryRaster.CloseFont;
begin
  if FFont <> nil then
    begin
      DisposeObject(FFont);
      FFont := nil;
    end;
end;

procedure TMemoryRaster.OpenAgg;
begin
  if Empty then
    begin
      FAggNeed := True;
      Exit;
    end;
  if FAggImage = nil then
      FAggImage := TMemoryRaster_AggImage.Create(Self);

  if FAgg = nil then
    begin
      FAgg := TMemoryRaster_Agg2D.Create(TAggPixelFormat.pfBGRA);
      FAgg.Attach(Self);
    end;
end;

procedure TMemoryRaster.CloseAgg;
begin
  FreeAgg;
  FAggNeed := False;
end;

function TMemoryRaster.AggActivted: Boolean;
begin
  Result := (FAggImage <> nil) and (FAgg <> nil);
end;

procedure TMemoryRaster.Clear;
begin
  Clear($FF000000);
end;

procedure TMemoryRaster.Clear(FillColor: TRasterColor);
begin
  if Empty then
      Exit;
  FillRasterColor(Bits^[0], width * height, FillColor);
end;

procedure TMemoryRaster.SetSize(NewWidth, NewHeight: Integer);
begin
  if (NewWidth = FWidth) and (NewHeight = FHeight) and (Assigned(FBits)) then
      Exit;

  if Assigned(FBits) then
    begin
      if FFreeBits then
          FreeMem(FBits);
      FBits := nil;
    end;

  CloseVertex;
  FreeAgg;

  FFreeBits := True;
  GetMem(FBits, NewWidth * NewHeight * SizeOf(TRasterColor));
  FWidth := NewWidth;
  FHeight := NewHeight;

  if FAggNeed then
      OpenAgg;
end;

procedure TMemoryRaster.SetSize(NewWidth, NewHeight: Integer; const ClearColor: TRasterColor);
begin
  SetSize(NewWidth, NewHeight);
  FillRasterColor(FBits^[0], NewWidth * NewHeight, ClearColor);
end;

procedure TMemoryRaster.SetSizeF(NewWidth, NewHeight: TGeoFloat; const ClearColor: TRasterColor);
begin
  SetSize(Round(NewWidth), Round(NewHeight), ClearColor);
end;

procedure TMemoryRaster.SetSizeR(r: TRectV2; const ClearColor: TRasterColor);
begin
  SetSizeF(RectWidth(r), RectHeight(r), ClearColor);
end;

function TMemoryRaster.SizeOfPoint: TPoint;
begin
  Result := Point(width, height);
end;

function TMemoryRaster.SizeOf2DPoint: TVec2;
begin
  Result := Make2DPoint(width, height);
end;

function TMemoryRaster.Size2D: TVec2;
begin
  Result := SizeOf2DPoint;
end;

function TMemoryRaster.Empty: Boolean;
begin
  Result := (FBits = nil) or (FWidth <= 0) or (FHeight <= 0);
end;

function TMemoryRaster.BoundsRect: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := width;
  Result.Bottom := height;
end;

function TMemoryRaster.BoundsRectV2: TRectV2;
begin
  Result := MakeRectV2(0, 0, width, height);
end;

function TMemoryRaster.Centroid: TVec2;
begin
  Result := Vec2(width * 0.5, height * 0.5);
end;

procedure TMemoryRaster.Reset;
begin
  if Assigned(FBits) then
    begin
      if FFreeBits then
          FreeMem(FBits);
      FBits := nil;
    end;

  if FFont <> nil then
    begin
      DisposeObject(FFont);
      FFont := nil;
    end;

  CloseVertex;
  FreeAgg;

  FFreeBits := True;
  FWidth := 0;
  FHeight := 0;
end;

procedure TMemoryRaster.Assign(sour: TMemoryRaster);
begin
  Reset;
  FWidth := sour.FWidth;
  FHeight := sour.FHeight;

  FDrawMode := sour.FDrawMode;
  FCombineMode := sour.FCombineMode;

  FMasterAlpha := sour.FMasterAlpha;
  FOuterColor := sour.FOuterColor;

  FFreeBits := True;
  GetMem(FBits, sour.FWidth * sour.FHeight * SizeOf(TRasterColor));
  CopyRasterColor(sour.FBits^[0], FBits^[0], sour.FWidth * sour.FHeight);

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.FlipHorz;
var
  i, j: Integer;
  p1, p2: PRasterColor;
  tmp: TRasterColor;
  w, W2: Integer;
begin
  w := width;
  { In-place flipping }
  p1 := PRasterColor(Bits);
  p2 := p1;
  inc(p2, width - 1);
  W2 := width shr 1;
  for j := 0 to height - 1 do
    begin
      for i := 0 to W2 - 1 do
        begin
          tmp := p1^;
          p1^ := p2^;
          p2^ := tmp;
          inc(p1);
          dec(p2);
        end;
      inc(p1, w - W2);
      inc(p2, w + W2);
    end;

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.FlipVert;
var
  j, j2: Integer;
  buffer: PRasterColorArray;
  p1, p2: PRasterColor;
begin
  { in-place }
  j2 := height - 1;
  GetMem(buffer, width shl 2);
  for j := 0 to height div 2 - 1 do
    begin
      p1 := PixelPtr[0, j];
      p2 := PixelPtr[0, j2];
      CopyRasterColor(p1^, buffer^, width);
      CopyRasterColor(p2^, p1^, width);
      CopyRasterColor(buffer^, p2^, width);
      dec(j2);
    end;
  FreeMem(buffer);

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.Rotate90;
var
  tmp: TMemoryRaster;
  x, y, i, j: Integer;
begin
  tmp := TMemoryRaster.Create;

  tmp.SetSize(height, width);
  i := 0;
  for y := 0 to height - 1 do
    begin
      j := height - 1 - y;
      for x := 0 to width - 1 do
        begin
          tmp.Bits^[j] := Bits^[i];
          inc(i);
          inc(j, height);
        end;
    end;

  Reset;
  FWidth := tmp.FWidth;
  FHeight := tmp.FHeight;
  FBits := tmp.FBits;

  tmp.FWidth := 0;
  tmp.FHeight := 0;
  tmp.FBits := nil;
  DisposeObject(tmp);

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.Rotate180;
var
  i, i2: Integer;
  tmp: TRasterColor;
begin
  i2 := width * height - 1;
  for i := 0 to width * height div 2 - 1 do
    begin
      tmp := Bits^[i2];
      Bits^[i2] := Bits^[i];
      Bits^[i] := tmp;
      dec(i2);
    end;

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.Rotate270;
var
  tmp: TMemoryRaster;
  x, y, i, j: Integer;
begin
  tmp := TMemoryRaster.Create;

  tmp.SetSize(height, width);
  i := 0;
  for y := 0 to height - 1 do
    begin
      j := (width - 1) * height + y;
      for x := 0 to width - 1 do
        begin
          tmp.Bits^[j] := Bits^[i];
          inc(i);
          dec(j, height);
        end;
    end;

  Reset;
  FWidth := tmp.FWidth;
  FHeight := tmp.FHeight;
  FBits := tmp.FBits;

  tmp.FWidth := 0;
  tmp.FHeight := 0;
  tmp.FBits := nil;
  DisposeObject(tmp);

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.Rotate(dest: TMemoryRaster; Angle: TGeoFloat; Endge: Integer);
begin
  ProjectionTo(dest,
    TV2Rect4.Init(BoundsRectV2, 0),
    TV2Rect4.Init(BoundsRectV2, Angle).ScaleToRect(dest.BoundsRectV2, Endge),
    True, 1.0);
end;

procedure TMemoryRaster.Rotate(Angle: TGeoFloat; Endge: Integer; BackgroundColor: TRasterColor);
var
  n: TMemoryRaster;
begin
  n := TMemoryRaster.Create;
  n.SetSize(width, height, BackgroundColor);
  Rotate(n, Angle, Endge);
  Reset;
  FWidth := n.width;
  FHeight := n.height;
  FBits := n.FBits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.CalibrateRotate(BackgroundColor: TRasterColor);
begin
  if Empty then
    Exit;
  Rotate(-DocmentRotationDetected(10, 128, Self), 1, BackgroundColor);
end;

procedure TMemoryRaster.CalibrateRotate;
begin
  CalibrateRotate(RasterColor(0, 0, 0, 0));
end;

procedure TMemoryRaster.NoLineZoomLine(const Source, dest: TMemoryRaster; const pass: Integer);
var
  j: Integer;
  SourceI, SourceJ: Double;
  SourceIInt, SourceJInt: Integer;
  SourceINext, SourceJNext: Integer;
begin
  for j := 0 to dest.height - 1 do
    begin
      SourceI := (pass / (dest.width - 1)) * (Source.width - 1);
      SourceJ := (j / (dest.height - 1)) * (Source.height - 1);

      SourceIInt := Trunc(SourceI);
      SourceJInt := Trunc(SourceJ);

      dest.Pixel[pass, j] := Source.Pixel[SourceIInt, SourceJInt]
    end;
end;

procedure TMemoryRaster.NoLineZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
{$IFDEF parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    NoLineZoomLine(Source, Self, pass);
  end;
{$ENDIF FPC}
{$ENDIF parallel}


var
  i: Integer;

begin
  if (Source.width = NewWidth) and (Source.height = NewHeight) then
    begin
      Assign(Source);
      Exit;
    end;
  SetSize(NewWidth, NewHeight);

  if (Source.width > 1) and (Source.width > 1) and (width > 1) and (height > 1) then
    begin
{$IFDEF parallel}
{$IFDEF FPC}
      ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, width - 1);
{$ELSE}
      TParallel.for(0, width - 1, procedure(pass: Integer)
        begin
          NoLineZoomLine(Source, Self, pass);
        end);
{$ENDIF FPC}
{$ELSE}
      for i := width - 1 downto 0 do
          NoLineZoomLine(Source, Self, i);
{$ENDIF parallel}
    end;
end;

procedure TMemoryRaster.NoLineZoom(const NewWidth, NewHeight: Integer);
var
  n: TMemoryRaster;
begin
  n := TMemoryRaster.Create;
  n.NoLineZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.width;
  FHeight := n.height;
  FBits := n.FBits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.ZoomLine(const Source, dest: TMemoryRaster; const pass: Integer);
var
  j: Integer;
  SourceI, SourceJ: Double;
begin
  for j := 0 to dest.height - 1 do
    begin
      SourceI := (pass / (dest.width - 1));
      SourceJ := (j / (dest.height - 1));

      dest.Pixel[pass, j] := Source.PixelWrapLinear[SourceI, SourceJ];
    end;
end;

procedure TMemoryRaster.ZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
{$IFDEF parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    ZoomLine(Source, Self, pass);
  end;
{$ENDIF FPC}
{$ENDIF parallel}


var
  i: Integer;

begin
  if (Source.width = NewWidth) and (Source.height = NewHeight) then
    begin
      Assign(Source);
      Exit;
    end;
  SetSize(NewWidth, NewHeight);

  if (Source.width > 1) and (Source.width > 1) and (width > 1) and (height > 1) then
    begin
{$IFDEF parallel}
{$IFDEF FPC}
      ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, width - 1);
{$ELSE}
      TParallel.for(0, width - 1, procedure(pass: Integer)
        begin
          ZoomLine(Source, Self, pass);
        end);
{$ENDIF FPC}
{$ELSE}
      for i := width - 1 downto 0 do
          ZoomLine(Source, Self, i);
{$ENDIF parallel}
    end;
end;

procedure TMemoryRaster.ZoomFrom(const Source: TMemoryRaster; const f: TGeoFloat);
begin
  ZoomFrom(Source, Round(Source.width * f), Round(Source.height * f));
end;

procedure TMemoryRaster.Zoom(const NewWidth, NewHeight: Integer);
var
  n: TMemoryRaster;
begin
  n := TMemoryRaster.Create;
  n.ZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.width;
  FHeight := n.height;
  FBits := n.FBits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.FastBlurZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
var
  k1, k2: Double;
  n: TMemoryRaster;
begin
  if (Source.width = NewWidth) and (Source.height = NewHeight) then
    begin
      Assign(Source);
      Exit;
    end;
  k1 := Max(NewWidth / Source.width, NewHeight / Source.height);
  k2 := Max(Source.width / NewWidth, Source.height / NewHeight);
  if (k1 < 1.0) then
    begin
      n := TMemoryRaster.Create;
      // preprocess zoom
      FastBlur(Source, n, k2 * 0.5, Source.BoundsRect);
      // zoom
      ZoomFrom(n, NewWidth, NewHeight);
      DisposeObject(n);
    end
  else
    begin
      // zoom
      n := TMemoryRaster.Create;
      // preprocess zoom
      FastBlur(Source, n, Min(2, k1 * 0.5), Source.BoundsRect);
      // zoom
      ZoomFrom(n, NewWidth, NewHeight);
      DisposeObject(n);
    end;
end;

procedure TMemoryRaster.FastBlurZoom(const NewWidth, NewHeight: Integer);
var
  n: TMemoryRaster;
begin
  n := TMemoryRaster.Create;
  n.FastBlurZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.width;
  FHeight := n.height;
  FBits := n.FBits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.GaussianBlurZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
var
  k1, k2: Double;
  n: TMemoryRaster;
begin
  if (Source.width = NewWidth) and (Source.height = NewHeight) then
    begin
      Assign(Source);
      Exit;
    end;

  k1 := Max(NewWidth / Source.width, NewHeight / Source.height);
  k2 := Max(Source.width / NewWidth, Source.height / NewHeight);
  if (k1 < 1.0) then
    begin
      n := TMemoryRaster.Create;
      // preprocess zoom
      GaussianBlur(Source, n, k2 * 0.5, Source.BoundsRect);
      // zoom
      ZoomFrom(n, NewWidth, NewHeight);
      DisposeObject(n);
    end
  else
    begin
      n := TMemoryRaster.Create;
      // preprocess zoom
      GaussianBlur(Source, n, Min(2, k1 * 0.5), Source.BoundsRect);
      // zoom
      ZoomFrom(n, NewWidth, NewHeight);
      DisposeObject(n);
    end;
end;

procedure TMemoryRaster.GaussianBlurZoom(const NewWidth, NewHeight: Integer);
var
  n: TMemoryRaster;
begin
  n := TMemoryRaster.Create;
  n.GaussianBlurZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.width;
  FHeight := n.height;
  FBits := n.FBits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.GrayscaleBlurZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
var
  k1, k2: Double;
  n: TMemoryRaster;
begin
  if (Source.width = NewWidth) and (Source.height = NewHeight) then
    begin
      Assign(Source);
      Grayscale;
      Exit;
    end;
  k1 := Max(NewWidth / Source.width, NewHeight / Source.height);
  k2 := Max(Source.width / NewWidth, Source.height / NewHeight);
  if (k1 < 1.0) then
    begin
      n := TMemoryRaster.Create;
      // preprocess zoom
      GrayscaleBlur(Source, n, k2 * 0.5, Source.BoundsRect);
      // zoom
      ZoomFrom(n, NewWidth, NewHeight);
      DisposeObject(n);
    end
  else
    begin
      n := TMemoryRaster.Create;
      // preprocess zoom
      GrayscaleBlur(Source, n, Min(2, k1 * 0.5), Source.BoundsRect);
      // zoom
      ZoomFrom(n, NewWidth, NewHeight);
      DisposeObject(n);
    end;
end;

procedure TMemoryRaster.GrayscaleBlurZoom(const NewWidth, NewHeight: Integer);
var
  n: TMemoryRaster;
begin
  n := TMemoryRaster.Create;
  n.GrayscaleBlurZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.width;
  FHeight := n.height;
  FBits := n.FBits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);

  CloseVertex;
  FreeAgg;
end;

procedure TMemoryRaster.Scale(k: TGeoFloat);
begin
  Zoom(Round(width * k), Round(height * k));
end;

function TMemoryRaster.FormatAsBGRA: TMemoryRaster;
var
  dest: TMemoryRaster;
  pass: Integer;
begin
  dest := TMemoryRaster.Create;
  dest.FFreeBits := True;
  GetMem(dest.FBits, width * height * SizeOf(TRasterColor));
  dest.FWidth := width;
  dest.FHeight := height;

  for pass := (width * height) - 1 downto 0 do
      dest.FBits^[pass] := RGBA2BGRA(FBits^[pass]);

  Result := dest;
end;

procedure TMemoryRaster.FormatBGRA;
var
  pass: Integer;
begin
  for pass := (width * height) - 1 downto 0 do
      SwapBR(FBits^[pass]);
end;

function TMemoryRaster.BuildRGB(cSwapBR: Boolean): PRGBArray;
begin
  Result := System.GetMemory(width * height * 3);
  OutputRGB(Result^, cSwapBR);
end;

procedure TMemoryRaster.InputRGB(var buff; w, h: Integer; cSwapBR: Boolean);
var
  pass: Integer;
  p: PByte;
begin
  if (width <> w) or (height <> h) then
      SetSize(w, h);
  p := @buff;
  for pass := 0 to (width * height) - 1 do
    begin
      FBits^[pass] := RGB2RGBA(PRGB(p)^);
      if cSwapBR then
          SwapBR(FBits^[pass]);
      inc(p, 3);
    end;
end;

procedure TMemoryRaster.OutputRGB(var buff; cSwapBR: Boolean);
var
  p: PByte;
  pass: Integer;
begin
  p := @buff;
  for pass := 0 to (width * height) - 1 do
    begin
      PRGB(p)^ := PRGB(@FBits^[pass])^;
      if cSwapBR then
          SwapBR(PRGB(p)^);
      inc(p, 3);
    end;
end;

procedure TMemoryRaster.ColorTransparent(c: TRasterColor);
var
  i, j: Integer;
  a: Byte;
  ce: TRasterColorEntry;
begin
  ce.BGRA := c;
  a := ce.a;
  for i := 0 to width - 1 do
    for j := 0 to height - 1 do
      begin
        ce.BGRA := Pixel[i, j];
        ce.a := a;
        if ce.BGRA = c then
            Pixel[i, j] := RasterColor(0, 0, 0, 0);
      end;
end;

procedure TMemoryRaster.ColorBlend(c: TRasterColor);
var
  i, j: Integer;
begin
  for i := 0 to width - 1 do
    for j := 0 to height - 1 do
        Pixel[i, j] := BlendReg(Pixel[i, j], c);
end;

procedure TMemoryRaster.Grayscale;
begin
  RGBToGrayscale(Self);
end;

procedure TMemoryRaster.ExtractGray(var output: TByteRaster);
var
  i, j: Integer;
begin
  SetLength(output, FHeight, FWidth);
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
        output[j, i] := PixelGray[i, j];
end;

procedure TMemoryRaster.ExtractRed(var output: TByteRaster);
var
  i, j: Integer;
begin
  SetLength(output, FHeight, FWidth);
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
        output[j, i] := PixelRed[i, j];
end;

procedure TMemoryRaster.ExtractGreen(var output: TByteRaster);
var
  i, j: Integer;
begin
  SetLength(output, FHeight, FWidth);
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
        output[j, i] := PixelGreen[i, j];
end;

procedure TMemoryRaster.ExtractBlue(var output: TByteRaster);
var
  i, j: Integer;
begin
  SetLength(output, FHeight, FWidth);
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
        output[j, i] := PixelBlue[i, j];
end;

procedure TMemoryRaster.ExtractAlpha(var output: TByteRaster);
var
  i, j: Integer;
begin
  SetLength(output, FHeight, FWidth);
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
        output[j, i] := PixelAlpha[i, j];
end;

function TMemoryRaster.ComputeAreaScaleSpace(clipArea: TRectV2; SS_width, SS_height: Integer): TRectV2;
var
  k: TGeoFloat;
  r: TRectV2;
  w, h, nw, nh: TGeoFloat;
  d: TVec2;
begin
  k := SS_width / SS_height;
  r := FixRect(clipArea);
  w := RectWidth(r);
  h := RectHeight(r);

  if w < h then
    begin
      nw := h * k;
      nh := h;
    end
  else
    begin
      nw := w;
      nh := w * k;
    end;

  d[0] := (nw - w) * 0.5;
  d[1] := (nh - h) * 0.5;
  r[0] := Vec2Sub(r[0], d);
  r[1] := Vec2Add(r[1], d);

  r := FixRect(r);
  r[0, 0] := Max(r[0, 0], 0);
  r[0, 1] := Max(r[0, 1], 0);
  r[1, 0] := Min(r[1, 0], width);
  r[1, 1] := Min(r[1, 1], height);

  Result := r;
end;

function TMemoryRaster.ComputeAreaScaleSpace(clipArea: TRect; SS_width, SS_height: Integer): TRect;
begin
  Result := MakeRect(ComputeAreaScaleSpace(RectV2(clipArea), SS_width, SS_height));
end;

function TMemoryRaster.BuildAreaOffsetScaleSpace(clipArea: TRectV2; SS_width, SS_height: Integer): TMemoryRaster;
begin
  Result := NewRaster();
  Result.SetSize(SS_width, SS_height);
  ProjectionTo(Result, TV2Rect4.Init(ComputeAreaScaleSpace(clipArea, SS_width, SS_height), 0), TV2Rect4.Init(Result.BoundsRectV2, 0), True, 1.0);
end;

function TMemoryRaster.BuildAreaOffsetScaleSpace(clipArea: TRect; SS_width, SS_height: Integer): TMemoryRaster;
begin
  Result := BuildAreaOffsetScaleSpace(RectV2(clipArea), SS_width, SS_height);
end;

function TMemoryRaster.BuildAreaCopy(clipArea: TRectV2): TMemoryRaster;
var
  r: TRectV2;
  w, h: TGeoFloat;
begin
  r := FixRect(clipArea);
  w := RectWidth(r);
  h := RectHeight(r);

  Result := NewRaster();
  Result.SetSize(Round(w), Round(h));
  ProjectionTo(Result, TV2Rect4.Init(r, 0), TV2Rect4.Init(Result.BoundsRectV2, 0), True, 1.0);
end;

function TMemoryRaster.BuildAreaCopy(clipArea: TRect): TMemoryRaster;
begin
  Result := BuildAreaCopy(RectV2(clipArea));
end;

procedure TMemoryRaster.Line(x1, y1, x2, y2: Integer; Value: TRasterColor; L: Boolean);
  procedure VertLine(x, y1, y2: Integer);
  var
    i, nh, NL: Integer;
    p: PRasterColor;
  begin
    if (x < 0) or (x >= width) then
        Exit;
    y1 := ClampInt_(y1, 0, height);
    y2 := ClampInt_(y2, 0, height);

    if y2 < y1 then
        CoreClasses.Swap(y1, y2);

    p := PixelPtr[x, y1];
    i := y2 - y1 + 1;
    nh := i shr 2;
    NL := i and $03;
    for i := 0 to nh - 1 do
      begin
        BlendMem(Value, p^);
        inc(p, width);
        BlendMem(Value, p^);
        inc(p, width);
        BlendMem(Value, p^);
        inc(p, width);
        BlendMem(Value, p^);
        inc(p, width);
      end;
    for i := 0 to NL - 1 do
      begin
        BlendMem(Value, p^);
        inc(p, width);
      end;
  end;

  procedure HorzLine(x1, y, x2: Integer);
  var
    i: Integer;
  begin
    if (y < 0) or (y >= height) then
        Exit;
    x1 := ClampInt_(x1, 0, width - 1);
    x2 := ClampInt_(x2, 0, width - 1);

    if x1 > x2 then
        CoreClasses.Swap(x1, x2);

    for i := x1 to x2 do
        BlendMem(Value, PixelPtr[i, y]^);
  end;

var
  dy, dx, SY, SX, i, Delta: Integer;
  pi, pl: Integer;
begin
  if FAgg <> nil then
    begin
      FAgg.LineColor := Value;
      FAgg.Line(x1, y1, x2, y2);
      Exit;
    end;

  try
    dx := x2 - x1;
    dy := y2 - y1;

    if dx > 0 then
        SX := 1
    else if dx < 0 then
      begin
        dx := -dx;
        SX := -1;
      end
    else // Dx = 0
      begin
        if dy > 0 then
            VertLine(x1, y1, y2 - 1)
        else if dy < 0 then
            VertLine(x1, y2 + 1, y1);
        if L then
            Pixel[x2, y2] := Value;
        Exit;
      end;

    if dy > 0 then
        SY := 1
    else if dy < 0 then
      begin
        dy := -dy;
        SY := -1;
      end
    else // Dy = 0
      begin
        if x2 > x1 then
            HorzLine(x1, y1, x2 - 1)
        else
            HorzLine(x2 + 1, y1, x1);
        if L then
            Pixel[x2, y2] := Value;
        Exit;
      end;

    pi := x1 + y1 * width;
    SY := SY * width;
    pl := width * height;

    if dx > dy then
      begin
        Delta := dx shr 1;
        for i := 0 to dx - 1 do
          begin
            if (pi >= 0) and (pi < pl) then
                BlendMem(Value, FBits^[pi]);

            inc(pi, SX);
            inc(Delta, dy);
            if Delta >= dx then
              begin
                inc(pi, SY);
                dec(Delta, dx);
              end;
          end;
      end
    else // Dx < Dy
      begin
        Delta := dy shr 1;
        for i := 0 to dy - 1 do
          begin
            if (pi >= 0) and (pi < pl) then
                BlendMem(Value, FBits^[pi]);

            inc(pi, SY);
            inc(Delta, dx);
            if Delta >= dy then
              begin
                inc(pi, SX);
                dec(Delta, dy);
              end;
          end;
      end;
    if (L) and (pi >= 0) and (pi < pl) then
        BlendMem(Value, FBits^[pi]);
  except
  end;
end;

procedure TMemoryRaster.LineF(x1, y1, x2, y2: TGeoFloat; Value: TRasterColor; L: Boolean);
begin
  Line(Round(x1), Round(y1), Round(x2), Round(y2), Value, L);
end;

procedure TMemoryRaster.LineF(p1, p2: TVec2; Value: TRasterColor; L: Boolean);
begin
  LineF(p1[0], p1[1], p2[0], p2[1], Value, L);
end;

procedure TMemoryRaster.LineF(p1, p2: TVec2; Value: TRasterColor; L, Cross: Boolean);
begin
  LineF(p1[0], p1[1], p2[0], p2[1], Value, L);
  if not Cross then
      Exit;
  DrawCrossF(p1, Round((FWidth + FHeight) div 2 * 0.02), RasterColorInv(Value));
  DrawCrossF(p2, Round((FWidth + FHeight) div 2 * 0.02), RasterColorInv(Value));
end;

procedure TMemoryRaster.FillRect(x1, y1, x2, y2: Integer; Value: TRasterColor);
var
  j, i: Integer;
  p: PRasterColor;
begin
  if FAgg <> nil then
    begin
      FAgg.FillColor := Value;
      FAgg.NoLine;
      FAgg.Rectangle(x1, y1, x2, y2);
    end
  else
    begin
      FixRect(x1, y1, x2, y2);

      if x1 < 0 then
          x1 := 0
      else if x1 >= width then
          x1 := width - 1;

      if x2 < 0 then
          x2 := 0
      else if x2 >= width then
          x2 := width - 1;

      if y1 < 0 then
          y1 := 0
      else if y1 >= height then
          y1 := height - 1;
      if y2 < 0 then
          y2 := 0
      else if y2 >= height then
          y2 := height - 1;

      for j := y1 to y2 - 1 do
        begin
          i := x1;
          p := @ScanLine[j]^[i];
          while i < x2 do
            begin
              BlendMem(Value, p^);
              inc(i);
              inc(p);
            end;
        end;
    end;
end;

procedure TMemoryRaster.FillRect(Dstx, Dsty, LineDist: Integer; Value: TRasterColor);
var
  l2, x1, y1, x2, y2: Integer;
begin
  l2 := LineDist div 2;
  x1 := Dstx - l2;
  y1 := Dsty - l2;
  x2 := Dstx + l2;
  y2 := Dsty + l2;
  FillRect(x1, y1, x2, y2, Value);
end;

procedure TMemoryRaster.FillRect(Dst: TVec2; LineDist: Integer; Value: TRasterColor);
begin
  FillRect(Round(Dst[0]), Round(Dst[1]), LineDist, Value);
end;

procedure TMemoryRaster.FillRect(r: TRectV2; Value: TRasterColor);
begin
  FillRect(Round(r[0, 0]), Round(r[0, 1]), Round(r[1, 0]), Round(r[1, 1]), Value);
end;

procedure TMemoryRaster.FillRect(r: TRectV2; Angle: TGeoFloat; Value: TRasterColor);
var
  a: TGeoFloat;
  r4: TV2Rect4;
  buff: array [0 .. 4] of TPointDouble;
begin
  a := NormalizeDegAngle(Angle);
  if a = 0 then
      FillRect(r, Value)
  else
    begin
      r4 := TV2Rect4.Init(r, a);
      if FAgg <> nil then
        begin
          buff[0].x := r4.LeftTop[0];
          buff[0].y := r4.LeftTop[1];
          buff[1].x := r4.RightTop[0];
          buff[1].y := r4.RightTop[1];
          buff[2].x := r4.RightBottom[0];
          buff[2].y := r4.RightBottom[1];
          buff[3].x := r4.LeftBottom[0];
          buff[3].y := r4.LeftBottom[1];
          buff[4].x := r4.LeftTop[0];
          buff[4].y := r4.LeftTop[1];

          FAgg.FillColor := Value;
          FAgg.NoLine;
          FAgg.Polygon(@buff[0], 5);
        end
      else
          Projection(r4, Value);
    end;
end;

procedure TMemoryRaster.DrawRect(r: TRect; Value: TRasterColor);
begin
  DrawRect(RectV2(r), Value);
end;

procedure TMemoryRaster.DrawRect(r: TRectV2; Value: TRasterColor);
begin
  if FAgg <> nil then
    begin
      FAgg.NoFill;
      FAgg.LineColor := Value;
      FAgg.Rectangle(r[0][0], r[0][1], r[1][0], r[1][1]);
    end
  else
    begin
      LineF(Vec2(r[0][0], r[0][1]), Vec2(r[1][0], r[0][1]), Value, True);
      LineF(Vec2(r[1][0], r[0][1]), Vec2(r[1][0], r[1][1]), Value, True);
      LineF(Vec2(r[1][0], r[1][1]), Vec2(r[0][0], r[1][1]), Value, True);
      LineF(Vec2(r[0][0], r[1][1]), Vec2(r[0][0], r[0][1]), Value, True);
    end;
end;

procedure TMemoryRaster.DrawRect(r: TV2Rect4; Value: TRasterColor);
var
  buff: array [0 .. 4] of TPointDouble;
begin
  if FAgg <> nil then
    begin
      buff[0].x := r.LeftTop[0];
      buff[0].y := r.LeftTop[1];
      buff[1].x := r.RightTop[0];
      buff[1].y := r.RightTop[1];
      buff[2].x := r.RightBottom[0];
      buff[2].y := r.RightBottom[1];
      buff[3].x := r.LeftBottom[0];
      buff[3].y := r.LeftBottom[1];
      buff[4].x := r.LeftTop[0];
      buff[4].y := r.LeftTop[1];

      FAgg.NoFill;
      FAgg.LineColor := Value;
      FAgg.Polyline(@buff[0], 4);
    end
  else
    begin
      LineF(r.LeftTop, r.RightTop, Value, True);
      LineF(r.RightTop, r.RightBottom, Value, True);
      LineF(r.RightBottom, r.LeftBottom, Value, True);
      LineF(r.LeftBottom, r.LeftTop, Value, True);
    end;
end;

procedure TMemoryRaster.DrawRect(r: TRectV2; Angle: TGeoFloat; Value: TRasterColor);
var
  r4: TV2Rect4;
  buff: array [0 .. 4] of TPointDouble;
begin
  r4 := TV2Rect4.Init(r, Angle);
  if FAgg <> nil then
    begin
      buff[0].x := r4.LeftTop[0];
      buff[0].y := r4.LeftTop[1];
      buff[1].x := r4.RightTop[0];
      buff[1].y := r4.RightTop[1];
      buff[2].x := r4.RightBottom[0];
      buff[2].y := r4.RightBottom[1];
      buff[3].x := r4.LeftBottom[0];
      buff[3].y := r4.LeftBottom[1];
      buff[4].x := r4.LeftTop[0];
      buff[4].y := r4.LeftTop[1];

      FAgg.NoFill;
      FAgg.LineColor := Value;
      FAgg.Polyline(@buff[0], 5);
    end
  else
    begin
      LineF(r4.LeftTop, r4.RightTop, Value, True);
      LineF(r4.RightTop, r4.RightBottom, Value, True);
      LineF(r4.RightBottom, r4.LeftBottom, Value, True);
      LineF(r4.LeftBottom, r4.LeftTop, Value, True);
    end;
end;

procedure TMemoryRaster.DrawTriangle_Render(t: TTriangle; Transform: Boolean; Value: TRasterColor; Cross: Boolean);
begin
  if Transform then
    begin
      LineF(Vec2Mul(t[0].Render, Size2D), Vec2Mul(t[1].Render, Size2D), Value, True, Cross);
      LineF(Vec2Mul(t[1].Render, Size2D), Vec2Mul(t[2].Render, Size2D), Value, True, Cross);
      LineF(Vec2Mul(t[2].Render, Size2D), Vec2Mul(t[0].Render, Size2D), Value, True, Cross);
    end
  else
    begin
      LineF(t[0].Render, t[1].Render, Value, True, Cross);
      LineF(t[1].Render, t[2].Render, Value, True, Cross);
      LineF(t[2].Render, t[0].Render, Value, True, Cross);
    end;
end;

procedure TMemoryRaster.DrawTriangle_Sampler(t: TTriangle; Transform: Boolean; Value: TRasterColor; Cross: Boolean);
begin
  if Transform then
    begin
      LineF(Vec2Mul(t[0].Sampler, Size2D), Vec2Mul(t[1].Sampler, Size2D), Value, True, Cross);
      LineF(Vec2Mul(t[1].Sampler, Size2D), Vec2Mul(t[2].Sampler, Size2D), Value, True, Cross);
      LineF(Vec2Mul(t[2].Sampler, Size2D), Vec2Mul(t[0].Sampler, Size2D), Value, True, Cross);
    end
  else
    begin
      LineF(t[0].Sampler, t[1].Sampler, Value, True, Cross);
      LineF(t[1].Sampler, t[2].Sampler, Value, True, Cross);
      LineF(t[2].Sampler, t[0].Sampler, Value, True, Cross);
    end;
end;

procedure TMemoryRaster.DrawCross(Dstx, Dsty, LineDist: Integer; Value: TRasterColor);
var
  L, x1, y1, x2, y2: Integer;
begin
  L := LineDist div 2;

  x1 := Dstx - L;
  y1 := Dsty - L;
  x2 := Dstx + L;
  y2 := Dsty + L;
  Line(x1, y1, x2, y2, Value, False);

  x1 := Dstx - L;
  y1 := Dsty + L;
  x2 := Dstx + L;
  y2 := Dsty - L;
  Line(x1, y1, x2, y2, Value, False);
end;

procedure TMemoryRaster.DrawCrossF(Dstx, Dsty, LineDist: TGeoFloat; Value: TRasterColor);
begin
  DrawCross(Round(Dstx), Round(Dsty), Round(LineDist), Value);
end;

procedure TMemoryRaster.DrawCrossF(Dst: TVec2; LineDist: TGeoFloat; Value: TRasterColor);
begin
  DrawCrossF(Dst[0], Dst[1], LineDist, Value);
end;

procedure TMemoryRaster.DrawPointListLine(pl: TVec2List; Value: TRasterColor; wasClose: Boolean);
var
  i: Integer;
  p1, p2: PVec2;
  buff: array of TPointDouble;
begin
  if pl.Count < 2 then
      Exit;

  if FAgg <> nil then
    begin
      i := pl.Count;
      if wasClose then
          inc(i);

      SetLength(buff, i);

      for i := 0 to pl.Count - 1 do
        begin
          p1 := pl[i];
          buff[i].x := p1^[0];
          buff[i].y := p1^[1];
        end;
      if wasClose then
        begin
          p1 := pl.First;
          buff[pl.Count].x := p1^[0];
          buff[pl.Count].y := p1^[1];
        end;

      FAgg.NoFill;
      FAgg.LineColor := Value;
      FAgg.Polyline(@buff[0], pl.Count + 1);
      SetLength(buff, 0);
    end
  else
    begin
      for i := 1 to pl.Count - 1 do
        begin
          p1 := pl[i - 1];
          p2 := pl[i];
          LineF(p1^, p2^, Value, True);
        end;
      if wasClose then
        begin
          p1 := pl.First;
          p2 := pl.Last;
          LineF(p1^, p2^, Value, True);
        end;
    end;
end;

procedure TMemoryRaster.DrawCircle(CC: TVec2; r: TGeoFloat; Value: TRasterColor);
var
  vl: TVec2List;
begin
  if FAgg <> nil then
    begin
      FAgg.NoFill;
      FAgg.LineColor := Value;
      FAgg.Circle(CC[0], CC[1], r);
    end
  else
    begin
      vl := TVec2List.Create;
      vl.AddCirclePoint(Round(r), CC, r);
      DrawPointListLine(vl, Value, True);
      DisposeObject(vl);
    end;
end;

procedure TMemoryRaster.FillCircle(CC: TVec2; r: TGeoFloat; Value: TRasterColor);
var
  vl: TVec2List;
begin
  if FAgg <> nil then
    begin
      FAgg.FillColor := Value;
      FAgg.NoLine;
      FAgg.Circle(CC[0], CC[1], r);
    end
  else
    begin
      vl := TVec2List.Create;
      vl.AddCirclePoint(Round(r), CC, r);
      Vertex.FillPoly(vl, CC, Value);
      DisposeObject(vl);
    end;
end;

procedure TMemoryRaster.DrawEllipse(CC: TVec2; xRadius, yRadius: TGeoFloat; Value: TRasterColor);
var
  i, n: Integer;
  s, c: TGeoFloatArray;
  vl: TVec2List;
begin
  if Round(xRadius) = Round(yRadius) then
      DrawCircle(CC, xRadius, Value)
  else if FAgg <> nil then
    begin
      FAgg.NoFill;
      FAgg.LineColor := Value;
      FAgg.Ellipse(CC[0], CC[1], xRadius, yRadius);
    end
  else
    begin
      n := Round(Max(xRadius, yRadius) * 0.1) + 5;
      SetLength(s, n);
      SetLength(c, n);
      dec(n);
      BuildSinCosCache(@s, @c, 0, 90);
      for i := 0 to n do
        begin
          s[i] := s[i] * yRadius;
          c[i] := c[i] * xRadius;
        end;
      vl := TVec2List.Create;
      // first quadrant (top right)
      for i := 0 to n do
          vl.Add(CC[0] + c[i], CC[1] - s[i]);
      // second quadrant (top left)
      for i := n - 1 downto 0 do
          vl.Add(CC[0] - c[i], CC[1] - s[i]);
      // third quadrant (bottom left)
      for i := 1 to n do
          vl.Add(CC[0] - c[i], CC[1] + s[i]);
      // fourth quadrant (bottom right)
      for i := n - 1 downto 0 do
          vl.Add(CC[0] + c[i], CC[1] + s[i]);
      SetLength(s, 0);
      SetLength(c, 0);
      DrawPointListLine(vl, Value, False);
      DisposeObject(vl);
    end;
end;

procedure TMemoryRaster.DrawEllipse(r: TRectV2; Value: TRasterColor);
var
  cen: TVec2;
begin
  cen := RectCentre(r);
  DrawEllipse(cen, abs(r[0][0] - cen[0]), abs(r[1][1] - cen[1]), Value);
end;

procedure TMemoryRaster.FillEllipse(CC: TVec2; xRadius, yRadius: TGeoFloat; Value: TRasterColor);
var
  i, n: Integer;
  s, c: TGeoFloatArray;
  vl: TVec2List;
begin
  if Round(xRadius) = Round(yRadius) then
      FillCircle(CC, xRadius, Value)
  else if FAgg <> nil then
    begin
      FAgg.FillColor := Value;
      FAgg.NoLine;
      FAgg.Ellipse(CC[0], CC[1], xRadius, yRadius);
    end
  else
    begin
      n := Round(Max(xRadius, yRadius) * 0.1) + 5;
      SetLength(s, n);
      SetLength(c, n);
      dec(n);
      BuildSinCosCache(@s, @c, 0, 90);
      for i := 0 to n do
        begin
          s[i] := s[i] * yRadius;
          c[i] := c[i] * xRadius;
        end;
      vl := TVec2List.Create;
      // first quadrant (top right)
      for i := 0 to n do
          vl.Add(CC[0] + c[i], CC[1] - s[i]);
      // second quadrant (top left)
      for i := n - 1 downto 0 do
          vl.Add(CC[0] - c[i], CC[1] - s[i]);
      // third quadrant (bottom left)
      for i := 1 to n do
          vl.Add(CC[0] - c[i], CC[1] + s[i]);
      // fourth quadrant (bottom right)
      for i := n - 1 downto 0 do
          vl.Add(CC[0] + c[i], CC[1] + s[i]);
      SetLength(s, 0);
      SetLength(c, 0);
      Vertex.FillPoly(vl, CC, Value);
      DisposeObject(vl);
    end;
end;

procedure TMemoryRaster.FillEllipse(r: TRectV2; Value: TRasterColor);
var
  cen: TVec2;
begin
  cen := RectCentre(r);
  DrawEllipse(cen, abs(r[0][0] - cen[0]), abs(r[1][1] - cen[1]), Value);
end;

procedure TMemoryRaster.FillTriangle(t1, t2, t3: TVec2; Value: TRasterColor);
begin
  Vertex.DrawTriangle(t1, t2, t3, Value);
end;

procedure TMemoryRaster.FillTriangle(t1, t2, t3: TPoint; Value: TRasterColor);
begin
  FillTriangle(Vec2(t1), Vec2(t1), Vec2(t1), Value);
end;

procedure TMemoryRaster.FillTriangle(t1, t2, t3: TPointf; Value: TRasterColor);
begin
  FillTriangle(Vec2(t1), Vec2(t1), Vec2(t1), Value);
end;

function TMemoryRaster.TextSize(Text: SystemString; siz: TGeoFloat): TVec2;
begin
  Result := Vec2Mul(Font.TextSize(Text), siz / Font.FontSize)
end;

procedure TMemoryRaster.DrawText(Text: SystemString; x, y: Integer; RotateVec: TVec2; Angle, alpha, siz: TGeoFloat; TextColor: TRasterColor);
var
  fSiz: TVec2;
  sc: TGeoFloat;
  fPt: TVec2;
begin
  fSiz := Font.TextSize(Text);
  sc := siz / Font.FontSize;
  fPt := Vec2Mul(Vec2Sub(fSiz, Vec2Mul(fSiz, sc)), RotateVec);
  Font.Draw(Text, Self, Vec2(x - fPt[0], y - fPt[1]), TextColor, True, alpha, RotateVec, Angle, sc);
end;

procedure TMemoryRaster.DrawText(Text: SystemString; x, y: Integer; siz: TGeoFloat; TextColor: TRasterColor);
begin
  DrawText(Text, x, y, Vec2(0.5, 0.5), 0, 1, siz, TextColor);
end;

procedure TMemoryRaster.ProjectionTo(Dst: TMemoryRaster; const sourRect, DestRect: TV2Rect4; const bilinear_sampling: Boolean; const alpha: Single);
begin
  Dst.Vertex.DrawRect(sourRect, DestRect, Self, bilinear_sampling, alpha);
end;

procedure TMemoryRaster.Projection(const DestRect: TV2Rect4; const COLOR: TRasterColor);
begin
  Vertex.DrawRect(DestRect, COLOR);
end;

procedure TMemoryRaster.Projection(sour: TMemoryRaster; const sourRect, DestRect: TV2Rect4; const bilinear_sampling: Boolean; const alpha: TGeoFloat);
begin
  Vertex.DrawRect(sourRect, DestRect, sour, bilinear_sampling, alpha);
end;

procedure TMemoryRaster.Draw(Src: TMemoryRaster);
begin
  Src.DrawTo(Self);
end;

procedure TMemoryRaster.Draw(Dstx, Dsty: Integer; Src: TMemoryRaster);
begin
  Src.DrawTo(Self, Dstx, Dsty);
end;

procedure TMemoryRaster.Draw(Dstx, Dsty: Integer; const SrcRect: TRect; Src: TMemoryRaster);
begin
  Src.DrawTo(Self, Dstx, Dsty, SrcRect);
end;

procedure TMemoryRaster.DrawTo(Dst: TMemoryRaster);
begin
  if (Dst.FAgg <> nil) and (FAggImage <> nil) then
      Dst.FAgg.TransformImage(FAggImage, 0, 0, FWidth, FHeight, 0, 0, FWidth, FHeight)
  else
      BlockTransfer(Dst, 0, 0, Dst.BoundsRect, Self, BoundsRect, DrawMode);
end;

procedure TMemoryRaster.DrawTo(Dst: TMemoryRaster; Dstx, Dsty: Integer; const SrcRect: TRect);
begin
  if (Dst.FAgg <> nil) and (FAggImage <> nil) then
      Dst.FAgg.TransformImage(FAggImage, SrcRect.Left, SrcRect.Top, SrcRect.Right, SrcRect.Bottom, Dstx, Dsty, FWidth, FHeight)
  else
      BlockTransfer(Dst, Dstx, Dsty, Dst.BoundsRect, Self, SrcRect, DrawMode);
end;

procedure TMemoryRaster.DrawTo(Dst: TMemoryRaster; Dstx, Dsty: Integer);
begin
  if (Dst.FAgg <> nil) and (FAggImage <> nil) then
      Dst.FAgg.TransformImage(FAggImage, 0, 0, FWidth, FHeight, Dstx, Dsty, FWidth, FHeight)
  else
      BlockTransfer(Dst, Dstx, Dsty, Dst.BoundsRect, Self, BoundsRect, DrawMode);
end;

procedure TMemoryRaster.DrawTo(Dst: TMemoryRaster; DstPt: TVec2);
begin
  DrawTo(Dst, Round(DstPt[0]), Round(DstPt[1]));
end;

class function TMemoryRaster.CanLoadStream(stream: TCoreClassStream): Boolean;
var
  bakPos: Int64;
  hflag, hflag2: Word;
  Header: TBmpHeader;
  j: TMemoryJpegRaster;
begin
  Result := False;
  try
    bakPos := stream.Position;

    stream.read(hflag, 2);
    if (hflag = $8D42) or (hflag = $8D43) or (hflag = $8D44) or (hflag = $8D46) or (hflag = $8D47) or (hflag = $8D48) then
        Result := True
    else if (hflag = $D8FF) or (hflag = $8DFF) then
      begin
        Result := True;
      end
    else
      begin
        stream.Position := bakPos;

        stream.ReadBuffer(Header, SizeOf(TBmpHeader));

        Result := (Header.bfType = $4D42) and (Header.biBitCount in [24, 32]) and (Header.biPlanes = 1) and (Header.biCompression = 0);
      end;

    stream.Position := bakPos;
  except
  end;
end;

procedure TMemoryRaster.LoadFromBmpStream(stream: TCoreClassStream);
var
  i, j, k, w: Integer;
  Header: TBmpHeader;
  tempBuff: TBytes;
begin
  Reset;

  stream.ReadBuffer(Header, SizeOf(TBmpHeader));

  // Check for Windows bitmap magic bytes and general compatibility of the
  // bitmap data that ought to be loaded...
  if (Header.bfType = $4D42) and (Header.biBitCount in [24, 32]) and (Header.biPlanes = 1) and (Header.biCompression = 0) then
    begin
      SetSize(Header.biWidth, abs(Header.biHeight));

      if Header.biBitCount = 32 then
        begin
          // 32bit bitmap
          // Check whether the bitmap is saved top-down
          if Header.biHeight > 0 then
            begin
              w := width * 4;
              for i := height - 1 downto 0 do
                begin
                  stream.ReadBuffer(ScanLine[i]^, w);
                end;
            end
          else
            begin
              stream.ReadBuffer(FBits^, width * height * 4);
            end;
        end
      else // 24bit bitmap
        begin
          w := FWidth * 3;
          k := w mod 4;
          if k <> 0 then
              inc(w, 4 - k);

          SetLength(tempBuff, w * FHeight);
          stream.ReadBuffer(tempBuff[0], w * FHeight);

          // Check whether the bitmap is saved top-down
          if Header.biHeight > 0 then
            begin
              k := 0;
              for j := height - 1 downto 0 do
                begin
                  for i := 0 to FWidth - 1 do
                      FBits^[i + j * FWidth] := RGB2RGBA(PRGB(@tempBuff[k + i * 3])^);
                  inc(k, w);
                end;
            end
          else
            begin
              k := 0;
              for j := 0 to height - 1 do
                begin
                  for i := 0 to FWidth - 1 do
                      FBits^[i + j * FWidth] := RGB2RGBA(PRGB(@tempBuff[k + i * 3])^);
                  inc(k, w);
                end;
            end;

          SetLength(tempBuff, 0);
        end;
    end
  else
    begin
      raise CoreClassException.Create('bmp format failed!');
    end;
end;

procedure TMemoryRaster.LoadFromStream(stream: TCoreClassStream);
var
  bakPos: Int64;

  hflag, hflag2: Word;
  m64: TMemoryStream64;
  j: TMemoryJpegRaster;
begin
  Reset;

  bakPos := stream.Position;

  stream.read(hflag, 2);
  if hflag = $8D42 then
    begin
      m64 := TMemoryStream64.Create;
      DecompressStream(stream, m64);
      m64.Position := 0;
      LoadFromBmpStream(m64);
      DisposeObject(m64);
      Exit;
    end
  else if hflag = $8D43 then
    begin
      m64 := TMemoryStream64.Create;
      DeflateDecompressStream(stream, m64);
      m64.Position := 0;
      LoadFromBmpStream(m64);
      DisposeObject(m64);
      Exit;
    end
  else if hflag = $8D44 then
    begin
      m64 := TMemoryStream64.Create;
      BRRCDecompressStream(stream, m64);
      m64.Position := 0;
      LoadFromBmpStream(m64);
      DisposeObject(m64);
      Exit;
    end
  else if hflag = $4D42 then
    begin
      stream.Position := bakPos;
      LoadFromBmpStream(stream);
    end
    // jls endian support
  else if (hflag = $D8FF) or (hflag = $8DFF) then
    begin
      stream.read(hflag2, 2);
      stream.Position := bakPos;
      if (hflag2 = $F7FF) then
          DecodeJpegLSRasterFromStream(stream, Self)
      else
        begin
          stream.Position := bakPos;

          m64 := TMemoryStream64.Create;
          if stream is TMemoryStream64 then
              m64.SetPointerWithProtectedMode(TMemoryStream64(stream).Memory, stream.Size - stream.Position)
          else if stream is TCoreClassMemoryStream then
              m64.SetPointerWithProtectedMode(TCoreClassMemoryStream(stream).Memory, stream.Size - stream.Position)
          else
              m64.CopyFrom(stream, stream.Size - stream.Position);

          m64.Position := 0;

          j := TMemoryJpegRaster.Create;
          j.Image.BitmapCS := TJpegColorSpace.jcRGBA;
          j.Image.StoredCS := TJpegColorSpace.jcAutoDetect;
          j.Scale := TJpegScale.jsFull;
          j.Performance := jpBestSpeed;
          try
            j.LoadFromStream(m64);
            j.GetRaster(Self);
            stream.Position := bakPos + m64.Position;
          except
          end;

          DisposeObject(m64);
          DisposeObject(j);
        end;
    end
    // yv12 format
  else if hflag = $8D46 then
    begin
      YV12ToRasterization(stream, Self);
      Exit;
    end
    // half yuv format
  else if hflag = $8D47 then
    begin
      HalfYUVToRasterization(stream, Self);
      Exit;
    end
    // quart yuv format
  else if hflag = $8D48 then
    begin
      QuartYUVToRasterization(stream, Self);
      Exit;
    end
  else
      stream.Position := bakPos;
end;

procedure TMemoryRaster.SaveToStream(stream: TCoreClassStream; RasterSave_: TRasterSave);
begin
  case RasterSave_ of
    rsRGBA: SaveToBmp32Stream(stream);
    rsRGB: SaveToBmp24Stream(stream);
    rsYV12: SaveToYV12Stream(stream);
    rsHalfYUV: SaveToHalfYUVStream(stream);
    rsQuartYUV: SaveToQuartYUVStream(stream);
    rsFastYV12: SaveToFastYV12Stream(stream);
    rsFastHalfYUV: SaveToFastHalfYUVStream(stream);
    rsFastQuartYUV: SaveToFastQuartYUVStream(stream);
    rsJpeg_RGBA_Qualily90: SaveToJPEGRGBAStream(stream, 90);
    rsJPEG_RGB_Qualily90: SaveToJPEGRGBStream(stream, 90);
    rsJPEG_Gray_Qualily90: SaveToJPEGGrayStream(stream, 90);
    rsJPEG_GrayA_Qualily90: SaveToJPEGGrayAStream(stream, 90);
    rsJpeg_RGBA_Qualily80: SaveToJPEGRGBAStream(stream, 80);
    rsJPEG_RGB_Qualily80: SaveToJPEGRGBStream(stream, 80);
    rsJPEG_Gray_Qualily80: SaveToJPEGGrayStream(stream, 80);
    rsJPEG_GrayA_Qualily80: SaveToJPEGGrayAStream(stream, 80);
    rsJpeg_RGBA_Qualily70: SaveToJPEGRGBAStream(stream, 70);
    rsJPEG_RGB_Qualily70: SaveToJPEGRGBStream(stream, 70);
    rsJPEG_Gray_Qualily70: SaveToJPEGGrayStream(stream, 70);
    rsJPEG_GrayA_Qualily70: SaveToJPEGGrayAStream(stream, 70);
    rsJpeg_RGBA_Qualily60: SaveToJPEGRGBAStream(stream, 60);
    rsJPEG_RGB_Qualily60: SaveToJPEGRGBStream(stream, 60);
    rsJPEG_Gray_Qualily60: SaveToJPEGGrayStream(stream, 60);
    rsJPEG_GrayA_Qualily60: SaveToJPEGGrayAStream(stream, 60);
    rsJpeg_RGBA_Qualily50: SaveToJPEGRGBAStream(stream, 50);
    rsJPEG_RGB_Qualily50: SaveToJPEGRGBStream(stream, 50);
    rsJPEG_Gray_Qualily50: SaveToJPEGGrayStream(stream, 50);
    rsJPEG_GrayA_Qualily50: SaveToJPEGGrayAStream(stream, 50);
  end;
end;

procedure TMemoryRaster.SaveToStream(stream: TCoreClassStream);
begin
  SaveToBmp32Stream(stream);
end;

procedure TMemoryRaster.SaveToBmp32Stream(stream: TCoreClassStream);
var
  Header: TBmpHeader;
  BitmapSize: Integer;
begin
  BitmapSize := (FWidth * FHeight) * 4;

  Header.bfType := $4D42;
  Header.bfSize := BitmapSize + SizeOf(TBmpHeader);
  Header.bfReserved1 := 0;
  Header.bfReserved2 := 0;
  // Save offset relative. However, the spec says it has to be file absolute,
  // which we can not do properly within a stream...
  Header.bfOffBits := SizeOf(TBmpHeader);
  Header.biSize := $28;
  Header.biWidth := width;

  Header.biHeight := -FHeight;

  Header.biPlanes := 1;
  Header.biBitCount := 32;
  Header.biCompression := 0;
  Header.biSizeImage := BitmapSize;
  Header.biXPelsPerMeter := 0;
  Header.biYPelsPerMeter := 0;
  Header.biClrUsed := 0;
  Header.biClrImportant := 0;

  stream.WriteBuffer(Header, SizeOf(TBmpHeader));

  stream.WriteBuffer(Bits^, BitmapSize);
end;

procedure TMemoryRaster.SaveToBmp24Stream(stream: TCoreClassStream);
var
  wSiz, M: Integer;
  Header: TBmpHeader;
  BitmapSize: Integer;
  tempBuff: TBytes;
  p: PRGB;
  i, j: Integer;
begin
  wSiz := FWidth * 3;
  M := wSiz mod 4;
  if M <> 0 then
      inc(wSiz, 4 - M);

  BitmapSize := wSiz * FHeight;

  Header.bfType := $4D42;
  Header.bfSize := BitmapSize + SizeOf(TBmpHeader);
  Header.bfReserved1 := 0;
  Header.bfReserved2 := 0;
  Header.bfOffBits := SizeOf(TBmpHeader);
  Header.biSize := $28;
  Header.biWidth := FWidth;
  Header.biHeight := FHeight;

  Header.biPlanes := 1;
  Header.biBitCount := 24;
  Header.biCompression := 0;
  Header.biSizeImage := BitmapSize;
  Header.biXPelsPerMeter := 0;
  Header.biYPelsPerMeter := 0;
  Header.biClrUsed := 0;
  Header.biClrImportant := 0;

  stream.WriteBuffer(Header, SizeOf(TBmpHeader));

  SetLength(tempBuff, wSiz);
  for i := FWidth * 3 to wSiz - 1 do
      tempBuff[i] := 0;

  for j := height - 1 downto 0 do
    begin
      p := PRGB(@tempBuff[0]);
      for i := 0 to FWidth - 1 do
        begin
          p^ := RGBA2RGB(FBits^[j * FWidth + i]);
          inc(p);
        end;
      stream.WriteBuffer(tempBuff[0], wSiz);
    end;

  SetLength(tempBuff, 0);
end;

procedure TMemoryRaster.SaveToZLibCompressStream(stream: TCoreClassStream);
var
  hflag: Word;
  m64: TMemoryStream64;
begin
  hflag := $8D42; // MemoryRaster compress format
  stream.write(hflag, 2);

  m64 := TMemoryStream64.Create;
  SaveToBmp24Stream(m64);
  m64.Position := 0;
  MaxCompressStream(m64, stream);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToDeflateCompressStream(stream: TCoreClassStream);
var
  hflag: Word;
  m64: TMemoryStream64;
begin
  hflag := $8D43; // MemoryRaster compress format
  stream.write(hflag, 2);

  m64 := TMemoryStream64.Create;
  SaveToBmp24Stream(m64);
  m64.Position := 0;
  DeflateCompressStream(m64, stream);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToBRRCCompressStream(stream: TCoreClassStream);
var
  hflag: Word;
  m64: TMemoryStream64;
begin
  hflag := $8D44; // MemoryRaster compress format
  stream.write(hflag, 2);

  m64 := TMemoryStream64.Create;
  SaveToBmp24Stream(m64);
  m64.Position := 0;
  BRRCCompressStream(m64, stream);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToJpegLS1Stream(stream: TCoreClassStream);
begin
  EncodeJpegLSRasterToStream1(Self, stream);
end;

procedure TMemoryRaster.SaveToJpegLS3Stream(stream: TCoreClassStream);
begin
  EncodeJpegLSRasterToStream3(Self, stream);
end;

procedure TMemoryRaster.SaveToYV12Stream(stream: TCoreClassStream);
var
  hflag: Word;
begin
  hflag := $8D46; // yv12 format
  stream.write(hflag, 2);
  RasterizationToYV12(True, Self, stream);
end;

procedure TMemoryRaster.SaveToFastYV12Stream(stream: TCoreClassStream);
var
  hflag: Word;
begin
  hflag := $8D46; // yv12 format
  stream.write(hflag, 2);
  RasterizationToYV12(False, Self, stream);
end;

procedure TMemoryRaster.SaveToHalfYUVStream(stream: TCoreClassStream);
var
  hflag: Word;
begin
  hflag := $8D47; // half yuv format
  stream.write(hflag, 2);
  RasterizationToHalfYUV(True, Self, stream);
end;

procedure TMemoryRaster.SaveToFastHalfYUVStream(stream: TCoreClassStream);
var
  hflag: Word;
begin
  hflag := $8D47; // yu12 format
  stream.write(hflag, 2);
  RasterizationToHalfYUV(False, Self, stream);
end;

procedure TMemoryRaster.SaveToQuartYUVStream(stream: TCoreClassStream);
var
  hflag: Word;
begin
  hflag := $8D48; // quart yuv format
  stream.write(hflag, 2);
  RasterizationToQuartYUV(True, Self, stream);
end;

procedure TMemoryRaster.SaveToFastQuartYUVStream(stream: TCoreClassStream);
var
  hflag: Word;
begin
  hflag := $8D48; // quart yuv format
  stream.write(hflag, 2);
  RasterizationToQuartYUV(False, Self, stream);
end;

procedure TMemoryRaster.SaveToJPEGRGBAStream(stream: TCoreClassStream; Quality: TJpegQuality);
var
  j: TMemoryJpegRaster;
  m64: TMemoryStream64;
begin
  j := TMemoryJpegRaster.Create;
  j.Image.BitmapCS := TJpegColorSpace.jcRGBA;
  j.Image.StoredCS := TJpegColorSpace.jcYCbCrA;
  j.CompressionQuality := Quality;
  j.Performance := jpBestSpeed;
  j.SetRaster(Self);

  m64 := TMemoryStream64.Create;
  j.SaveToStream(m64);
  stream.write(m64.Memory^, m64.Size);
  DisposeObject(m64);

  DisposeObject(j);
end;

procedure TMemoryRaster.SaveToJPEGRGBStream(stream: TCoreClassStream; Quality: TJpegQuality);
var
  j: TMemoryJpegRaster;
  m64: TMemoryStream64;
begin
  j := TMemoryJpegRaster.Create;
  j.Image.BitmapCS := TJpegColorSpace.jcRGBA;
  j.Image.StoredCS := TJpegColorSpace.jcYCbCr;
  j.CompressionQuality := Quality;
  j.Performance := jpBestSpeed;
  j.SetRaster(Self);

  m64 := TMemoryStream64.Create;
  j.SaveToStream(m64);
  stream.write(m64.Memory^, m64.Size);
  DisposeObject(m64);

  DisposeObject(j);
end;

procedure TMemoryRaster.SaveToJpegCMYKRGBStream(stream: TCoreClassStream; Quality: TJpegQuality);
var
  j: TMemoryJpegRaster;
  m64: TMemoryStream64;
begin
  j := TMemoryJpegRaster.Create;
  j.Image.BitmapCS := TJpegColorSpace.jcRGBA;
  j.Image.StoredCS := TJpegColorSpace.jcCMYK;
  j.CompressionQuality := Quality;
  j.Performance := jpBestSpeed;
  j.SetRaster(Self);

  m64 := TMemoryStream64.Create;
  j.SaveToStream(m64);
  stream.write(m64.Memory^, m64.Size);
  DisposeObject(m64);

  DisposeObject(j);
end;

procedure TMemoryRaster.SaveToJPEGGrayStream(stream: TCoreClassStream; Quality: TJpegQuality);
var
  j: TMemoryJpegRaster;
  m64: TMemoryStream64;
begin
  j := TMemoryJpegRaster.Create;
  j.Image.BitmapCS := TJpegColorSpace.jcRGBA;
  j.Image.StoredCS := TJpegColorSpace.jcGray;
  j.CompressionQuality := Quality;
  j.Performance := jpBestSpeed;
  j.SetRaster(Self);

  m64 := TMemoryStream64.Create;
  j.SaveToStream(m64);
  stream.write(m64.Memory^, m64.Size);
  DisposeObject(m64);

  DisposeObject(j);
end;

procedure TMemoryRaster.SaveToJPEGGrayAStream(stream: TCoreClassStream; Quality: TJpegQuality);
var
  j: TMemoryJpegRaster;
  m64: TMemoryStream64;
begin
  j := TMemoryJpegRaster.Create;
  j.Image.BitmapCS := TJpegColorSpace.jcRGBA;
  j.Image.StoredCS := TJpegColorSpace.jcGrayA;
  j.CompressionQuality := Quality;
  j.Performance := jpBestSpeed;
  j.SetRaster(Self);

  m64 := TMemoryStream64.Create;
  j.SaveToStream(m64);
  stream.write(m64.Memory^, m64.Size);
  DisposeObject(m64);

  DisposeObject(j);
end;

class function TMemoryRaster.CanLoadFile(fn: SystemString): Boolean;
var
  m64: TCoreClassFileStream;
begin
  m64 := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  try
      Result := CanLoadStream(m64);
  except
      Result := False;
  end;
  DisposeObject(m64);
end;

procedure TMemoryRaster.LoadFromFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
    m64.LoadFromFile(fn);
    m64.Position := 0;
    LoadFromStream(m64);
  except
  end;
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToBmp32File(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToBmp32Stream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToBmp24File(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToBmp24Stream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToStream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToZLibCompressFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToZLibCompressStream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToDeflateCompressFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToDeflateCompressStream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToBRRCCompressFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToBRRCCompressStream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToJpegLS1File(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToJpegLS1Stream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToJpegLS3File(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToJpegLS3Stream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToYV12File(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToYV12Stream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToFastYV12File(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToFastYV12Stream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToHalfYUVFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToHalfYUVStream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToFastHalfYUVFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToFastHalfYUVStream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToQuartYUVFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToQuartYUVStream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToFastQuartYUVFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToFastQuartYUVStream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToJpegRGBAFile(fn: SystemString; Quality: TJpegQuality);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToJPEGRGBAStream(m64, Quality);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToJpegRGBFile(fn: SystemString; Quality: TJpegQuality);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToJPEGRGBStream(m64, Quality);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToJpegCMYKRGBFile(fn: SystemString; Quality: TJpegQuality);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToJpegCMYKRGBStream(m64, Quality);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToJpegGrayFile(fn: SystemString; Quality: TJpegQuality);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToJPEGGrayStream(m64, Quality);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToJpegGrayAFile(fn: SystemString; Quality: TJpegQuality);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToJPEGGrayAStream(m64, Quality);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;
