procedure MakeMergeTables;
var
  i, j: Integer;
const
  OneByteth: Double = 1.0 / $FF;
begin
  for j := 0 to $FF do
    for i := 0 to $FF do
      begin
        DivTable[i, j] := Byte(Round(i * j * OneByteth));
        if i > 0 then
            RcTable[i, j] := Byte(Round(j * $FF / i))
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

function TMemoryRaster.GetBits: PRColorArray;
begin
  ReadyBits();
  FActivted := True;
  Result := FBits;
end;

function TMemoryRaster.GetPixel(const x, y: Integer): TRColor;
begin
  Result := PixelPtr[x, y]^;
end;

procedure TMemoryRaster.SetPixel(const x, y: Integer; const Value: TRColor);
begin
  PixelPtr[x, y]^ := Value;
end;

function TMemoryRaster.GetPixelBGRA(const x, y: Integer): TRColor;
begin
  Result := RGBA2BGRA(GetPixel(x, y));
end;

procedure TMemoryRaster.SetPixelBGRA(const x, y: Integer; const Value: TRColor);
begin
  SetPixel(x, y, BGRA2RGBA(Value));
end;

function TMemoryRaster.GetPixelPtr(const x, y: Integer): PRColor;
begin
  Result := @(Bits^[ClampInt(x, 0, width - 1) + ClampInt(y, 0, height - 1) * width]);
end;

function TMemoryRaster.GetScanLine(y: Integer): PRColorArray;
begin
  Result := @Bits^[y * FWidth];
end;

function TMemoryRaster.GetWidth0: TGeoFloat;
begin
  Result := FWidth - 1;
end;

function TMemoryRaster.GetHeight0: TGeoFloat;
begin
  Result := FHeight - 1;
end;

function TMemoryRaster.GetWidth0i: Integer;
begin
  Result := FWidth - 1;
end;

function TMemoryRaster.GetHeight0i: Integer;
begin
  Result := FHeight - 1;
end;

function TMemoryRaster.GetPixelRed(const x, y: Integer): Byte;
begin
  Result := PRColorEntry(GetPixelPtr(x, y))^.r;
end;

procedure TMemoryRaster.SetPixelRed(const x, y: Integer; const Value: Byte);
begin
  PRColorEntry(GetPixelPtr(x, y))^.r := Value;
end;

function TMemoryRaster.GetPixelGreen(const x, y: Integer): Byte;
begin
  Result := PRColorEntry(GetPixelPtr(x, y))^.g;
end;

procedure TMemoryRaster.SetPixelGreen(const x, y: Integer; const Value: Byte);
begin
  PRColorEntry(GetPixelPtr(x, y))^.g := Value;
end;

function TMemoryRaster.GetPixelBlue(const x, y: Integer): Byte;
begin
  Result := PRColorEntry(GetPixelPtr(x, y))^.b;
end;

procedure TMemoryRaster.SetPixelBlue(const x, y: Integer; const Value: Byte);
begin
  PRColorEntry(GetPixelPtr(x, y))^.b := Value;
end;

function TMemoryRaster.GetPixelAlpha(const x, y: Integer): Byte;
begin
  Result := PRColorEntry(GetPixelPtr(x, y))^.a;
end;

procedure TMemoryRaster.SetPixelAlpha(const x, y: Integer; const Value: Byte);
begin
  PRColorEntry(GetPixelPtr(x, y))^.a := Value;
end;

function TMemoryRaster.GetGray(const x, y: Integer): Byte;
begin
  Result := RColor2Gray(GetPixel(x, y));
end;

procedure TMemoryRaster.SetGray(const x, y: Integer; const Value: Byte);
begin
  SetPixel(x, y, RColor(Value, Value, Value, $FF));
end;

function TMemoryRaster.GetGrayS(const x, y: Integer): Single;
begin
  Result := RColor2GrayS(GetPixel(x, y));
end;

procedure TMemoryRaster.SetGrayS(const x, y: Integer; const Value: Single);
begin
  SetGray(x, y, ClampByte3(Round(Value * $FF), 0, $FF));
end;

function TMemoryRaster.GetGrayD(const x, y: Integer): Double;
begin
  Result := RColor2GrayD(GetPixel(x, y));
end;

procedure TMemoryRaster.SetGrayD(const x, y: Integer; const Value: Double);
begin
  SetGrayS(x, y, Value);
end;

function TMemoryRaster.GetPixelF(const x, y: TGeoFloat): TRColor;
begin
  Result := GetPixel(Round(x), Round(y));
end;

procedure TMemoryRaster.SetPixelF(const x, y: TGeoFloat; const Value: TRColor);
begin
  SetPixel(Round(x), Round(y), Value);
end;

function TMemoryRaster.GetPixelVec(const v2: TVec2): TRColor;
begin
  Result := GetPixelF(v2[0], v2[1]);
end;

procedure TMemoryRaster.SetPixelVec(const v2: TVec2; const Value: TRColor);
begin
  SetPixelF(v2[0], v2[1], Value)
end;

function TMemoryRaster.GetPixelLinearF(const x, y: TGeoFloat): TRColor;
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
  c1, c2, c3, c4: TRColorEntry;
  k1, k2, k3, K4: TGeoFloat;
  r, g, b, a: TGeoFloat;
begin
  fx := CoordWrap(x) * Width0;
  fy := CoordWrap(y) * Height0;

  i_x := ClampInt(Trunc(fx), 0, Width0i);
  i_y := ClampInt(Trunc(fy), 0, Height0i);

  i_x2 := i_x + 1;
  i_y2 := i_y + 1;
  if (i_x2 > Width0) or (i_y2 > Height0) then
    begin
      Result := Pixel[i_x, i_y];
      exit;
    end;

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

  TRColorEntry(Result).r := Round(r * $FF);
  TRColorEntry(Result).g := Round(g * $FF);
  TRColorEntry(Result).b := Round(b * $FF);
  TRColorEntry(Result).a := Round(a * $FF);
end;

function TMemoryRaster.GetPixelLinear(const x, y: Integer): TRColor;
begin
  Result := GetPixelLinearF(x / Width0, y / Height0);
end;

constructor TMemoryRaster.Create;
begin
  inherited Create;

  FSerializedEngine := nil;
  FMemorySerializedPosition := -1;
  FActivted := True;
  FActiveTimeTick := 0;

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
  if FSerializedEngine <> nil then
      FSerializedEngine.Remove(Self);
  CloseFont;
  CloseVertex;
  CloseAgg;
  RecycleMemory;
  inherited Destroy;
end;

function TMemoryRaster.ActiveTimeTick: TTimeTick;
begin
  if FActivted then
    begin
      FActiveTimeTick := GetTimeTick();
      FActivted := False;
    end;
  Result := FActiveTimeTick;
end;

function TMemoryRaster.SerializedAndRecycleMemory(RSeri: TRasterSerialized): Int64;
begin
  FSerializedEngine := RSeri;
  Result := SerializedAndRecycleMemory();
end;

function TMemoryRaster.SerializedAndRecycleMemory(): Int64;
begin
  if FSerializedEngine <> nil then
      Result := FSerializedEngine.Write(Self)
  else
      Result := 0;
end;

function TMemoryRaster.UnserializedMemory(RSeri: TRasterSerialized): Int64;
begin
  FSerializedEngine := RSeri;
  Result := UnserializedMemory();
end;

function TMemoryRaster.UnserializedMemory(): Int64;
begin
  if FSerializedEngine <> nil then
      Result := FSerializedEngine.Read(Self)
  else
      Result := 0;
end;

function TMemoryRaster.RecycleMemory(): Int64;
begin
  Result := 0;
  if Assigned(FBits) and FFreeBits then
    begin
      Result := FWidth * FHeight * 4;
      System.FreeMemory(FBits);
    end;
  FBits := nil;
end;

procedure TMemoryRaster.ReadyBits;
begin
  if (FBits = nil) and (FWidth > 0) and (FHeight > 0) and (FMemorySerializedPosition >= 0) and (FSerializedEngine <> nil) then
      UnserializedMemory();
end;

procedure TMemoryRaster.SetWorkMemory(WorkMemory: Pointer; NewWidth, NewHeight: Integer);
begin
  CloseVertex;
  FreeAgg;

  RecycleMemory;

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
      FBits := PRColorArray(WorkMemory);
      FWidth := NewWidth;
      FHeight := NewHeight;
    end;

  if FAggNeed then
      OpenAgg;

  FMemorySerializedPosition := -1;
  FSerializedEngine := nil;
end;

procedure TMemoryRaster.SetWorkMemory(raster: TMemoryRaster);
begin
  SetWorkMemory(@raster.Bits^[0], raster.width, raster.height);
end;

function TMemoryRaster.IsMemoryMap: Boolean;
begin
  Result := (FWidth > 0) and (FHeight > 0) and (not FFreeBits);
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
  if empty then
    begin
      FAggNeed := True;
      exit;
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

function TMemoryRaster.AggActivted: Boolean;
begin
  Result := (FAggImage <> nil) and (FAgg <> nil);
end;

procedure TMemoryRaster.Reset;
begin
  if FFont <> nil then
    begin
      DisposeObject(FFont);
      FFont := nil;
    end;

  CloseVertex;
  FreeAgg;
  RecycleMemory;

  FFreeBits := True;
  FWidth := 0;
  FHeight := 0;

  FMemorySerializedPosition := -1;
  FSerializedEngine := nil;
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

  CloseVertex;
  FreeAgg;

  FFreeBits := True;
  FAggNeed := sour.FAggNeed;

  FBits := System.GetMemory(sour.FWidth * sour.FHeight * SizeOf(TRColor));
  CopyRColor(sour.Bits^[0], FBits^[0], sour.FWidth * sour.FHeight);

  if FAggNeed then
      OpenAgg;
  FMemorySerializedPosition := -1;
  FSerializedEngine := nil;
end;

procedure TMemoryRaster.Clear;
begin
  Clear($FF000000);
end;

procedure TMemoryRaster.Clear(FillColor: TRColor);
begin
  if empty then
      exit;
  FillRColor(Bits^[0], width * height, FillColor);
end;

function TMemoryRaster.MemorySize: Integer;
begin
  Result := width * height * SizeOf(TRColor);
end;

procedure TMemoryRaster.SetSize(NewWidth, NewHeight: Integer);
begin
  if (NewWidth = FWidth) and (NewHeight = FHeight) and (FBits <> nil) then
      exit;

  CloseVertex;
  FreeAgg;
  RecycleMemory;
  FFreeBits := True;
  FBits := System.GetMemory(NewWidth * NewHeight * SizeOf(TRColor));
  FWidth := NewWidth;
  FHeight := NewHeight;

  if FAggNeed then
      OpenAgg;

  FMemorySerializedPosition := -1;
  FSerializedEngine := nil;
end;

procedure TMemoryRaster.SetSize(NewWidth, NewHeight: Integer; const ClearColor: TRColor);
begin
  SetSize(NewWidth, NewHeight);
  FillRColor(Bits^[0], NewWidth * NewHeight, ClearColor);
end;

procedure TMemoryRaster.SetSizeF(NewWidth, NewHeight: TGeoFloat; const ClearColor: TRColor);
begin
  SetSize(Round(NewWidth), Round(NewHeight), ClearColor);
end;

procedure TMemoryRaster.SetSizeR(r: TRectV2; const ClearColor: TRColor);
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
  Result := Make2DPoint(width, height);
end;

function TMemoryRaster.Size0: TVec2;
begin
  Result := Make2DPoint(Width0, Height0);
end;

function TMemoryRaster.empty: Boolean;
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

function TMemoryRaster.BoundsV2Rect4: TV2Rect4;
begin
  Result := TV2Rect4.Init(BoundsRectV2, 0);
end;

function TMemoryRaster.Centroid: TVec2;
begin
  Result := Vec2(width * 0.5, height * 0.5);
end;

function TMemoryRaster.Centre: TVec2;
begin
  Result := Vec2(width * 0.5, height * 0.5);
end;

function TMemoryRaster.InHere(const x, y: Integer): Boolean;
begin
  Result := PointInRect(x, y, 0, 0, width - 1, height - 1);
end;

procedure TMemoryRaster.FlipHorz;
var
  i, j: Integer;
  p1, p2: PRColor;
  tmp: TRColor;
  w, W2: Integer;
begin
  CloseVertex;
  FreeAgg;

  w := width;
  { In-place flipping }
  p1 := PRColor(Bits);
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

  if FAggNeed then
      OpenAgg;
  FMemorySerializedPosition := -1;
  FSerializedEngine := nil;
end;

procedure TMemoryRaster.FlipVert;
var
  j, j2: Integer;
  buffer: PRColorArray;
  p1, p2: PRColor;
begin
  CloseVertex;
  FreeAgg;
  { in-place }
  j2 := height - 1;
  buffer := System.GetMemory(width shl 2);
  for j := 0 to height div 2 - 1 do
    begin
      p1 := PixelPtr[0, j];
      p2 := PixelPtr[0, j2];
      CopyRColor(p1^, buffer^, width);
      CopyRColor(p2^, p1^, width);
      CopyRColor(buffer^, p2^, width);
      dec(j2);
    end;
  System.FreeMemory(buffer);

  if FAggNeed then
      OpenAgg;
  FMemorySerializedPosition := -1;
  FSerializedEngine := nil;
end;

procedure TMemoryRaster.Rotate90;
var
  tmp: TMemoryRaster;
  x, y, i, j: Integer;
begin
  CloseVertex;
  FreeAgg;

  ReadyBits();

  tmp := TMemoryRaster.Create;

  tmp.SetSize(height, width);
  i := 0;
  for y := 0 to height - 1 do
    begin
      j := height - 1 - y;
      for x := 0 to width - 1 do
        begin
          tmp.FBits^[j] := FBits^[i];
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

  if FAggNeed then
      OpenAgg;
  FMemorySerializedPosition := -1;
  FSerializedEngine := nil;
end;

procedure TMemoryRaster.Rotate180;
var
  i, i2: Integer;
  tmp: TRColor;
begin
  ReadyBits();
  i2 := width * height - 1;
  for i := 0 to width * height div 2 - 1 do
    begin
      tmp := FBits^[i2];
      FBits^[i2] := FBits^[i];
      FBits^[i] := tmp;
      dec(i2);
    end;
end;

procedure TMemoryRaster.Rotate270;
var
  tmp: TMemoryRaster;
  x, y, i, j: Integer;
begin
  CloseVertex;
  FreeAgg;

  ReadyBits();

  tmp := TMemoryRaster.Create;

  tmp.SetSize(height, width);
  i := 0;
  for y := 0 to height - 1 do
    begin
      j := (width - 1) * height + y;
      for x := 0 to width - 1 do
        begin
          tmp.FBits^[j] := FBits^[i];
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

  if FAggNeed then
      OpenAgg;
  FMemorySerializedPosition := -1;
  FSerializedEngine := nil;
end;

procedure TMemoryRaster.Rotate(dest: TMemoryRaster; Angle: TGeoFloat; Endge: Integer);
begin
  ProjectionTo(dest,
    TV2Rect4.Init(BoundsRectV2, 0),
    TV2Rect4.Init(BoundsRectV2, Angle).ScaleToRect(dest.BoundsRectV2, Endge),
    True, 1.0);
end;

procedure TMemoryRaster.Rotate(Angle: TGeoFloat; Endge: Integer; BackgroundColor: TRColor);
var
  n: TMemoryRaster;
  v: TV2Rect4;
  r: TRectV2;
begin
  CloseVertex;
  FreeAgg;

  v := TV2Rect4.Init(BoundsRectV2, Angle);
  r := v.BoundRect;

  n := TMemoryRaster.Create;
  n.SetSizeF(RectWidth(r), RectHeight(r), BackgroundColor);
  Rotate(n, Angle, Endge);
  Reset;
  FWidth := n.width;
  FHeight := n.height;
  FBits := n.Bits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);

  if FAggNeed then
      OpenAgg;
  FMemorySerializedPosition := -1;
  FSerializedEngine := nil;
end;

procedure TMemoryRaster.CalibrateRotate(BackgroundColor: TRColor);
begin
  if empty then
      exit;
  Rotate(-DocmentRotationDetected(60, $FF, Self), 0, BackgroundColor);
end;

procedure TMemoryRaster.CalibrateRotate;
begin
  CalibrateRotate(RColor(0, 0, 0, 0));
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
      SourceI := pass / dest.Width0 * Source.Width0;
      SourceJ := j / dest.Height0 * Source.Height0;

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
      exit;
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
  CloseVertex;
  FreeAgg;

  n := TMemoryRaster.Create;
  n.NoLineZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.width;
  FHeight := n.height;
  FBits := n.Bits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);

  if FAggNeed then
      OpenAgg;
  FMemorySerializedPosition := -1;
  FSerializedEngine := nil;
end;

procedure TMemoryRaster.ZoomLine(const Source, dest: TMemoryRaster; const pass: Integer);
var
  j: Integer;
  SourceI, SourceJ: Double;
begin
  for j := 0 to dest.height - 1 do
    begin
      SourceI := pass / dest.Width0;
      SourceJ := j / dest.Height0;

      dest.Pixel[pass, j] := Source.PixelLinearF[SourceI, SourceJ];
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
      exit;
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
  CloseVertex;
  FreeAgg;

  n := TMemoryRaster.Create;
  n.ZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.width;
  FHeight := n.height;
  FBits := n.Bits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);

  if FAggNeed then
      OpenAgg;
  FMemorySerializedPosition := -1;
  FSerializedEngine := nil;
end;

procedure TMemoryRaster.FastBlurZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
var
  k1, k2: Double;
  n: TMemoryRaster;
begin
  if (Source.width = NewWidth) and (Source.height = NewHeight) then
    begin
      Assign(Source);
      exit;
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
  CloseVertex;
  FreeAgg;

  n := TMemoryRaster.Create;
  n.FastBlurZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.width;
  FHeight := n.height;
  FBits := n.Bits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);

  if FAggNeed then
      OpenAgg;
  FMemorySerializedPosition := -1;
  FSerializedEngine := nil;
end;

procedure TMemoryRaster.GaussianBlurZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
var
  k1, k2: Double;
  n: TMemoryRaster;
begin
  if (Source.width = NewWidth) and (Source.height = NewHeight) then
    begin
      Assign(Source);
      exit;
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
  CloseVertex;
  FreeAgg;

  n := TMemoryRaster.Create;
  n.GaussianBlurZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.width;
  FHeight := n.height;
  FBits := n.Bits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);

  if FAggNeed then
      OpenAgg;
  FMemorySerializedPosition := -1;
  FSerializedEngine := nil;
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
      exit;
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
  CloseVertex;
  FreeAgg;

  n := TMemoryRaster.Create;
  n.GrayscaleBlurZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.width;
  FHeight := n.height;
  FBits := n.Bits;

  n.FBits := nil;
  n.FWidth := 0;
  n.FHeight := 0;
  DisposeObject(n);

  if FAggNeed then
      OpenAgg;
  FMemorySerializedPosition := -1;
  FSerializedEngine := nil;
end;

procedure TMemoryRaster.Scale(k: TGeoFloat);
begin
  Zoom(Round(width * k), Round(height * k));
end;

procedure TMemoryRaster.NoLineScale(k: TGeoFloat);
begin
  NoLineZoom(Round(width * k), Round(height * k));
end;

procedure TMemoryRaster.FitScale(NewWidth, NewHeight: TGeoFloat);
var
  r: TRectV2;
begin
  r := FitRect(BoundsRectV2, RectV2(0, 0, NewWidth, NewHeight));
  Zoom(Round(RectWidth(r)), Round(RectHeight(r)));
end;

function TMemoryRaster.FormatAsBGRA: TMemoryRaster;
var
  dest: TMemoryRaster;
  pass: Integer;
begin
  ReadyBits();
  dest := TMemoryRaster.Create;
  dest.SetSize(width, height);
  for pass := (width * height) - 1 downto 0 do
      dest.FBits^[pass] := RGBA2BGRA(FBits^[pass]);

  Result := dest;
end;

procedure TMemoryRaster.FormatBGRA;
var
  pass: Integer;
begin
  ReadyBits();
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

  ReadyBits();
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
  ReadyBits();
  p := @buff;
  for pass := 0 to (width * height) - 1 do
    begin
      PRGB(p)^ := PRGB(@FBits^[pass])^;
      if cSwapBR then
          SwapBR(PRGB(p)^);
      inc(p, 3);
    end;
end;

procedure TMemoryRaster.ColorTransparent(c: TRColor);
var
  i, j: Integer;
  a: Byte;
  ce: TRColorEntry;
begin
  ce.BGRA := c;
  a := ce.a;
  for i := 0 to width - 1 do
    for j := 0 to height - 1 do
      begin
        ce.BGRA := Pixel[i, j];
        ce.a := a;
        if ce.BGRA = c then
            Pixel[i, j] := RColor(0, 0, 0, 0);
      end;
end;

procedure TMemoryRaster.ColorBlend(c: TRColor);
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

function TMemoryRaster.ExistsColor(c: TRColor): Boolean;
var
  i, j: Integer;
begin
  Result := True;
  for j := 0 to height - 1 do
    for i := 0 to width - 1 do
      if Pixel[i, j] = c then
          exit;
  Result := False;
end;

function TMemoryRaster.FindFirstColor(c: TRColor): TPoint;
var
  i, j: Integer;
begin
  Result := Point(-1, -1);
  for j := 0 to height - 1 do
    for i := 0 to width - 1 do
      if Pixel[i, j] = c then
        begin
          Result := Point(i, j);
          exit;
        end;
end;

function TMemoryRaster.FindLastColor(c: TRColor): TPoint;
var
  i, j: Integer;
begin
  Result := Point(-1, -1);
  for j := height - 1 downto 0 do
    for i := width - 1 downto 0 do
      if Pixel[i, j] = c then
        begin
          Result := Point(i, j);
          exit;
        end;
end;

function TMemoryRaster.FindNearColor(c: TRColor; Pt: TVec2): TPoint;
var
  i, j: Integer;
  npt: TVec2;
  d1, d2: TGeoFloat;
begin
  Result := Point(-1, -1);
  npt := Vec2(-1, -1);
  d2 := Vec2Distance(npt, Pt);
  d1 := d2;

  for j := 0 to height - 1 do
    for i := 0 to width - 1 do
      if Pixel[i, j] = c then
        begin
          d2 := Vec2Distance(Vec2(i, j), Pt);
          if d2 < d1 then
            begin
              npt := Vec2(i, j);
              d1 := d2;
            end;
        end;

  if (npt[0] >= 0) and (npt[1] >= 0) then
      Result := MakePoint(npt);
end;

function TMemoryRaster.ColorBoundsRectV2(c: TRColor): TRectV2;
var
  color_inited_: Boolean;
  i, j: Integer;
begin
  color_inited_ := False;
  Result := NullRectV2;
  for j := 0 to height - 1 do
    for i := 0 to width - 1 do
      if Pixel[i, j] = c then
        begin
          if not color_inited_ then
            begin
              color_inited_ := True;
              Result[0] := Vec2(i, j);
              Result[1] := Result[0];
            end
          else
              Result := BoundRect(Result[0], Result[1], Vec2(i, j));
        end;
end;

function TMemoryRaster.ColorBoundsRect(c: TRColor): TRect;
var
  color_inited_: Boolean;
  i, j: Integer;
begin
  color_inited_ := False;
  Result := Rect(0, 0, 0, 0);
  for j := 0 to height - 1 do
    for i := 0 to width - 1 do
      if Pixel[i, j] = c then
        begin
          if not color_inited_ then
            begin
              color_inited_ := True;
              Result.TopLeft := Point(i, j);
              Result.Bottomright := Result.TopLeft;
            end
          else
              Result := BoundRect(Result.TopLeft, Result.Bottomright, Point(i, j));
        end;
end;

function TMemoryRaster.NoneColorBoundsRectV2(c: TRColor): TRectV2;
var
  color_inited_: Boolean;
  i, j: Integer;
begin
  color_inited_ := False;
  Result := NullRectV2;
  for j := 0 to height - 1 do
    for i := 0 to width - 1 do
      if Pixel[i, j] <> c then
        begin
          if not color_inited_ then
            begin
              color_inited_ := True;
              Result[0] := Vec2(i, j);
              Result[1] := Result[0];
            end
          else
              Result := BoundRect(Result[0], Result[1], Vec2(i, j));
        end;
end;

function TMemoryRaster.NoneColorBoundsRect(c: TRColor): TRect;
var
  color_inited_: Boolean;
  i, j: Integer;
begin
  color_inited_ := False;
  Result := Rect(0, 0, 0, 0);
  for j := 0 to height - 1 do
    for i := 0 to width - 1 do
      if Pixel[i, j] <> c then
        begin
          if not color_inited_ then
            begin
              color_inited_ := True;
              Result.TopLeft := Point(i, j);
              Result.Bottomright := Result.TopLeft;
            end
          else
              Result := BoundRect(Result.TopLeft, Result.Bottomright, Point(i, j));
        end;
end;

procedure TMemoryRaster.Line(x1, y1, x2, y2: Integer; Color: TRColor; L: Boolean);
  procedure VertLine(x, y1, y2: Integer);
  var
    i, nh, NL: Integer;
    p: PRColor;
  begin
    if (x < 0) or (x >= width) then
        exit;
    y1 := ClampInt(y1, 0, height);
    y2 := ClampInt(y2, 0, height);

    if y2 < y1 then
        CoreClasses.Swap(y1, y2);

    p := PixelPtr[x, y1];
    i := y2 - y1 + 1;
    nh := i shr 2;
    NL := i and $03;
    for i := 0 to nh - 1 do
      begin
        BlendMem(Color, p^);
        inc(p, width);
        BlendMem(Color, p^);
        inc(p, width);
        BlendMem(Color, p^);
        inc(p, width);
        BlendMem(Color, p^);
        inc(p, width);
      end;
    for i := 0 to NL - 1 do
      begin
        BlendMem(Color, p^);
        inc(p, width);
      end;
  end;

  procedure HorzLine(x1, y, x2: Integer);
  var
    i: Integer;
    p: PRColor;
  begin
    if (y < 0) or (y >= height) then
        exit;
    x1 := ClampInt(x1, 0, width - 1);
    x2 := ClampInt(x2, 0, width - 1);

    if x1 > x2 then
        CoreClasses.Swap(x1, x2);

    p := PixelPtr[x1, y];

    for i := x1 to x2 do
      begin
        BlendMem(Color, p^);
        inc(p);
      end;
  end;

var
  dy, dx, SY, SX, i, Delta: Integer;
  pi, pl: Integer;
begin
  if FAgg <> nil then
    begin
      FAgg.LineColor := Color;
      FAgg.Line(x1, y1, x2, y2);
      exit;
    end;

  ReadyBits();

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
            Pixel[x2, y2] := Color;
        exit;
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
            Pixel[x2, y2] := Color;
        exit;
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
                BlendMem(Color, FBits^[pi]);

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
                BlendMem(Color, FBits^[pi]);

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
        BlendMem(Color, FBits^[pi]);
  except
  end;
end;

procedure TMemoryRaster.LineF(x1, y1, x2, y2: TGeoFloat; Color: TRColor; L: Boolean);
begin
  Line(Round(x1), Round(y1), Round(x2), Round(y2), Color, L);
end;

procedure TMemoryRaster.LineF(p1, p2: TVec2; Color: TRColor; L: Boolean);
begin
  LineF(p1[0], p1[1], p2[0], p2[1], Color, L);
end;

procedure TMemoryRaster.LineF(p1, p2: TVec2; Color: TRColor; L, Cross: Boolean);
begin
  LineF(p1[0], p1[1], p2[0], p2[1], Color, L);
  if not Cross then
      exit;
  DrawCrossF(p1, Round((FWidth + FHeight) div 2 * 0.02), RColorInv(Color));
  DrawCrossF(p2, Round((FWidth + FHeight) div 2 * 0.02), RColorInv(Color));
end;

procedure TMemoryRaster.FillRect(x1, y1, x2, y2: Integer; Color: TRColor);
var
  j, i: Integer;
  p: PRColor;
begin
  if FAgg <> nil then
    begin
      FAgg.FillColor := Color;
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
              BlendMem(Color, p^);
              inc(i);
              inc(p);
            end;
        end;
    end;
end;

procedure TMemoryRaster.FillRect(Dstx, Dsty, LineDist: Integer; Color: TRColor);
var
  l2, x1, y1, x2, y2: Integer;
begin
  l2 := LineDist div 2;
  x1 := Dstx - l2;
  y1 := Dsty - l2;
  x2 := Dstx + l2;
  y2 := Dsty + l2;
  FillRect(x1, y1, x2, y2, Color);
end;

procedure TMemoryRaster.FillRect(Dst: TVec2; LineDist: Integer; Color: TRColor);
begin
  FillRect(Round(Dst[0]), Round(Dst[1]), LineDist, Color);
end;

procedure TMemoryRaster.FillRect(r: TRectV2; Color: TRColor);
begin
  FillRect(Round(r[0, 0]), Round(r[0, 1]), Round(r[1, 0]), Round(r[1, 1]), Color);
end;

procedure TMemoryRaster.FillRect(r: TRectV2; Angle: TGeoFloat; Color: TRColor);
var
  a: TGeoFloat;
  r4: TV2Rect4;
  buff: array [0 .. 4] of TPointDouble;
begin
  a := NormalizeDegAngle(Angle);
  if a = 0 then
      FillRect(r, Color)
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

          FAgg.FillColor := Color;
          FAgg.NoLine;
          FAgg.Polygon(@buff[0], 5);
        end
      else
          Projection(r4, Color);
    end;
end;

procedure TMemoryRaster.DrawRect(r: TRect; Color: TRColor);
begin
  DrawRect(RectV2(r), Color);
end;

procedure TMemoryRaster.DrawRect(r: TRectV2; Color: TRColor);
begin
  if FAgg <> nil then
    begin
      FAgg.NoFill;
      FAgg.LineColor := Color;
      FAgg.Rectangle(r[0][0], r[0][1], r[1][0], r[1][1]);
    end
  else
    begin
      LineF(Vec2(r[0][0], r[0][1]), Vec2(r[1][0], r[0][1]), Color, True);
      LineF(Vec2(r[1][0], r[0][1]), Vec2(r[1][0], r[1][1]), Color, True);
      LineF(Vec2(r[1][0], r[1][1]), Vec2(r[0][0], r[1][1]), Color, True);
      LineF(Vec2(r[0][0], r[1][1]), Vec2(r[0][0], r[0][1]), Color, True);
    end;
end;

procedure TMemoryRaster.DrawRect(r: TV2Rect4; Color: TRColor);
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
      FAgg.LineColor := Color;
      FAgg.Polyline(@buff[0], 5);
    end
  else
    begin
      LineF(r.LeftTop, r.RightTop, Color, True);
      LineF(r.RightTop, r.RightBottom, Color, True);
      LineF(r.RightBottom, r.LeftBottom, Color, True);
      LineF(r.LeftBottom, r.LeftTop, Color, True);
    end;
end;

procedure TMemoryRaster.DrawRect(r: TRectV2; Angle: TGeoFloat; Color: TRColor);
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
      FAgg.LineColor := Color;
      FAgg.Polyline(@buff[0], 5);
    end
  else
    begin
      LineF(r4.LeftTop, r4.RightTop, Color, True);
      LineF(r4.RightTop, r4.RightBottom, Color, True);
      LineF(r4.RightBottom, r4.LeftBottom, Color, True);
      LineF(r4.LeftBottom, r4.LeftTop, Color, True);
    end;
end;

procedure TMemoryRaster.DrawTriangle(tri: TTriangle; Transform: Boolean; Color: TRColor; Cross: Boolean);
begin
  if Transform then
    begin
      LineF(Vec2Mul(tri[0], Size0), Vec2Mul(tri[1], Size0), Color, True, Cross);
      LineF(Vec2Mul(tri[1], Size0), Vec2Mul(tri[2], Size0), Color, True, Cross);
      LineF(Vec2Mul(tri[2], Size0), Vec2Mul(tri[0], Size0), Color, True, Cross);
    end
  else
    begin
      LineF(tri[0], tri[1], Color, True, Cross);
      LineF(tri[1], tri[2], Color, True, Cross);
      LineF(tri[2], tri[0], Color, True, Cross);
    end;
end;

procedure TMemoryRaster.DrawCross(Dstx, Dsty, LineDist: Integer; Color: TRColor);
var
  L, x1, y1, x2, y2: Integer;
begin
  L := LineDist div 2;

  x1 := Dstx - L;
  y1 := Dsty - L;
  x2 := Dstx + L;
  y2 := Dsty + L;
  Line(x1, y1, x2, y2, Color, False);

  x1 := Dstx - L;
  y1 := Dsty + L;
  x2 := Dstx + L;
  y2 := Dsty - L;
  Line(x1, y1, x2, y2, Color, False);
end;

procedure TMemoryRaster.DrawCrossF(Dstx, Dsty, LineDist: TGeoFloat; Color: TRColor);
begin
  DrawCross(Round(Dstx), Round(Dsty), Round(LineDist), Color);
end;

procedure TMemoryRaster.DrawCrossF(Dst: TVec2; LineDist: TGeoFloat; Color: TRColor);
begin
  DrawCrossF(Dst[0], Dst[1], LineDist, Color);
end;

procedure TMemoryRaster.DrawPointListLine(pl: TVec2List; Color: TRColor; wasClose: Boolean);
var
  i: Integer;
  p1, p2: PVec2;
  buff: array of TPointDouble;
begin
  if pl.Count < 2 then
      exit;

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
      FAgg.LineColor := Color;
      FAgg.Polyline(@buff[0], pl.Count + 1);
      SetLength(buff, 0);
    end
  else
    begin
      for i := 1 to pl.Count - 1 do
        begin
          p1 := pl[i - 1];
          p2 := pl[i];
          LineF(p1^, p2^, Color, True);
        end;
      if wasClose then
        begin
          p1 := pl.First;
          p2 := pl.Last;
          LineF(p1^, p2^, Color, True);
        end;
    end;
end;

procedure TMemoryRaster.DrawCircle(CC: TVec2; r: TGeoFloat; Color: TRColor);
var
  vl: TVec2List;
begin
  if FAgg <> nil then
    begin
      FAgg.NoFill;
      FAgg.LineColor := Color;
      FAgg.Circle(CC[0], CC[1], r);
    end
  else
    begin
      vl := TVec2List.Create;
      vl.AddCirclePoint(Round(r), CC, r);
      DrawPointListLine(vl, Color, True);
      DisposeObject(vl);
    end;
end;

procedure TMemoryRaster.FillCircle(CC: TVec2; r: TGeoFloat; Color: TRColor);
var
  vl: TVec2List;
begin
  if FAgg <> nil then
    begin
      FAgg.FillColor := Color;
      FAgg.NoLine;
      FAgg.Circle(CC[0], CC[1], r);
    end
  else
    begin
      vl := TVec2List.Create;
      vl.AddCirclePoint(Round(r), CC, r);
      Vertex.FillPoly(vl, CC, Color);
      DisposeObject(vl);
    end;
end;

procedure TMemoryRaster.DrawEllipse(CC: TVec2; xRadius, yRadius: TGeoFloat; Color: TRColor);
var
  i, n: Integer;
  s, c: TGeoFloatArray;
  vl: TVec2List;
begin
  if Round(xRadius) = Round(yRadius) then
      DrawCircle(CC, xRadius, Color)
  else if FAgg <> nil then
    begin
      FAgg.NoFill;
      FAgg.LineColor := Color;
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
      DrawPointListLine(vl, Color, False);
      DisposeObject(vl);
    end;
end;

procedure TMemoryRaster.DrawEllipse(r: TRectV2; Color: TRColor);
var
  cen: TVec2;
begin
  cen := RectCentre(r);
  DrawEllipse(cen, abs(r[0][0] - cen[0]), abs(r[1][1] - cen[1]), Color);
end;

procedure TMemoryRaster.FillEllipse(CC: TVec2; xRadius, yRadius: TGeoFloat; Color: TRColor);
var
  i, n: Integer;
  s, c: TGeoFloatArray;
  vl: TVec2List;
begin
  if Round(xRadius) = Round(yRadius) then
      FillCircle(CC, xRadius, Color)
  else if FAgg <> nil then
    begin
      FAgg.FillColor := Color;
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
      Vertex.FillPoly(vl, CC, Color);
      DisposeObject(vl);
    end;
end;

procedure TMemoryRaster.FillEllipse(r: TRectV2; Color: TRColor);
var
  cen: TVec2;
begin
  cen := RectCentre(r);
  DrawEllipse(cen, abs(r[0][0] - cen[0]), abs(r[1][1] - cen[1]), Color);
end;

procedure TMemoryRaster.FillTriangle(t1, t2, t3: TVec2; Color: TRColor);
begin
  Vertex.DrawTriangle(t1, t2, t3, Color);
end;

procedure TMemoryRaster.FillTriangle(t1, t2, t3: TPoint; Color: TRColor);
begin
  FillTriangle(Vec2(t1), Vec2(t1), Vec2(t1), Color);
end;

procedure TMemoryRaster.FillTriangle(t1, t2, t3: TPointf; Color: TRColor);
begin
  FillTriangle(Vec2(t1), Vec2(t1), Vec2(t1), Color);
end;

procedure TMemoryRaster.FillPolygon(PolygonBuff: TArrayVec2; Color: TRColor);
  procedure Fill_Agg;
  var
    L, i: Integer;
    p1, p2: PVec2;
    buff: array of TPointDouble;
  begin
    L := Length(PolygonBuff);
    SetLength(buff, L + 1);

    for i := 0 to L - 1 do
      begin
        p1 := @PolygonBuff[i];
        buff[i].x := p1^[0];
        buff[i].y := p1^[1];
      end;

    p1 := @PolygonBuff[0];
    buff[L].x := p1^[0];
    buff[L].y := p1^[1];

    FAgg.NoLine;
    FAgg.FillColor := Color;
    FAgg.Polygon(@buff[0], L + 1, dpfFillOnly);
    SetLength(buff, 0);
  end;

  procedure Fill_Geo;
  var
    r: TRectV2;
{$IFDEF parallel}
{$IFDEF FPC}
    procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
    var
      p: PRColorArray;
      i: Integer;
      Pt: TVec2;
    begin
      p := ScanLine[pass];
      for i := 0 to width - 1 do
        begin
          Pt := Vec2(i, pass);
          if PointInRect(Pt, r) and PointInPolygon(Pt, PolygonBuff) then
              p^[i] := Color;
        end;
    end;
{$ENDIF FPC}
{$ELSE parallel}
    procedure DoFor;
    var
      p: PRColorArray;
      pass, i: Integer;
      Pt: TVec2;
    begin
      for pass := 0 to height - 1 do
        begin
          p := ScanLine[pass];
          for i := 0 to width - 1 do
            begin
              Pt := Vec2(i, pass);
              if PointInRect(Pt, r) and PointInPolygon(Pt, PolygonBuff) then
                  p^[i] := Color;
            end;
        end;
    end;
{$ENDIF parallel}

  begin
    r := BoundRect(PolygonBuff);
{$IFDEF parallel}
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, height - 1);
{$ELSE}
    TParallel.for(0, height - 1, procedure(pass: Integer)
      var
        p: PRColorArray;
        i: Integer;
        Pt: TVec2;
      begin
        p := ScanLine[pass];
        for i := 0 to width - 1 do
          begin
            Pt := Vec2(i, pass);
            if PointInRect(Pt, r) and PointInPolygon(Pt, PolygonBuff) then
                p^[i] := Color;
          end;
      end);
{$ENDIF FPC}
{$ELSE}
    DoFor();
{$ENDIF parallel}
  end;

begin
  if Length(PolygonBuff) < 2 then
      exit;

  if FAgg <> nil then
      Fill_Agg()
  else
      Fill_Geo();
end;

procedure TMemoryRaster.DrawPolygon(PolygonBuff: TArrayVec2; Color: TRColor);
var
  L, i: Integer;
  p1, p2: PVec2;
  buff: array of TPointDouble;
begin
  L := Length(PolygonBuff);
  if L < 2 then
      exit;

  if FAgg <> nil then
    begin
      SetLength(buff, L + 1);

      for i := 0 to L - 1 do
        begin
          p1 := @PolygonBuff[i];
          buff[i].x := p1^[0];
          buff[i].y := p1^[1];
        end;

      p1 := @PolygonBuff[0];
      buff[L].x := p1^[0];
      buff[L].y := p1^[1];

      FAgg.NoFill;
      FAgg.LineColor := Color;
      FAgg.Polygon(@buff[0], L + 1, dpfStrokeOnly);
      SetLength(buff, 0);
    end
  else
    begin
      for i := 1 to L - 1 do
        begin
          p1 := @PolygonBuff[i - 1];
          p2 := @PolygonBuff[i];
          LineF(p1^, p2^, Color, True);
        end;

      p1 := @PolygonBuff[0];
      p2 := @PolygonBuff[L - 1];
      LineF(p1^, p2^, Color, True);
    end;
end;

procedure TMemoryRaster.FillPolygon(Polygon: T2DPolygon; Color: TRColor);
  procedure Fill_Agg;
  var
    L, i: Integer;
    p1, p2: PVec2;
    buff: array of TPointDouble;
  begin
    L := Polygon.Count;
    SetLength(buff, L + 1);

    for i := 0 to L - 1 do
      begin
        p1 := Polygon[i];
        buff[i].x := p1^[0];
        buff[i].y := p1^[1];
      end;

    p1 := Polygon[0];
    buff[L].x := p1^[0];
    buff[L].y := p1^[1];

    FAgg.NoLine;
    FAgg.FillColor := Color;
    FAgg.Polygon(@buff[0], L + 1, dpfFillOnly);
    SetLength(buff, 0);
  end;

  procedure Fill_Geo;
  var
    r: TRectV2;
{$IFDEF parallel}
{$IFDEF FPC}
    procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
    var
      p: PRColorArray;
      i: Integer;
      Pt: TVec2;
    begin
      p := ScanLine[pass];
      for i := 0 to width - 1 do
        begin
          Pt := Vec2(i, pass);
          if PointInRect(Pt, r) and Polygon.InHere(Pt) then
              p^[i] := Color;
        end;
    end;
{$ENDIF FPC}
{$ELSE parallel}
    procedure DoFor;
    var
      p: PRColorArray;
      pass, i: Integer;
      Pt: TVec2;
    begin
      for pass := 0 to height - 1 do
        begin
          p := ScanLine[pass];
          for i := 0 to width - 1 do
            begin
              Pt := Vec2(i, pass);
              if PointInRect(Pt, r) and Polygon.InHere(Pt) then
                  p^[i] := Color;
            end;
        end;
    end;
{$ENDIF parallel}

  begin
    r := Polygon.BoundRect();
{$IFDEF parallel}
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, height - 1);
{$ELSE}
    TParallel.for(0, height - 1, procedure(pass: Integer)
      var
        p: PRColorArray;
        i: Integer;
        Pt: TVec2;
      begin
        p := ScanLine[pass];
        for i := 0 to width - 1 do
          begin
            Pt := Vec2(i, pass);
            if PointInRect(Pt, r) and Polygon.InHere(Pt) then
                p^[i] := Color;
          end;
      end);
{$ENDIF FPC}
{$ELSE}
    DoFor();
{$ENDIF parallel}
  end;

begin
  if Polygon.Count < 2 then
      exit;

  if FAgg <> nil then
      Fill_Agg()
  else
      Fill_Geo();
end;

procedure TMemoryRaster.DrawPolygon(Polygon: T2DPolygon; Color: TRColor);
var
  L, i: Integer;
  p1, p2: PVec2;
  buff: array of TPointDouble;
begin
  L := Polygon.Count;
  if L < 2 then
      exit;

  if FAgg <> nil then
    begin
      SetLength(buff, L + 1);

      for i := 0 to L - 1 do
        begin
          p1 := Polygon[i];
          buff[i].x := p1^[0];
          buff[i].y := p1^[1];
        end;

      p1 := Polygon[0];
      buff[L].x := p1^[0];
      buff[L].y := p1^[1];

      FAgg.NoFill;
      FAgg.LineColor := Color;
      FAgg.Polygon(@buff[0], L + 1, dpfStrokeOnly);
      SetLength(buff, 0);
    end
  else
    begin
      for i := 1 to L - 1 do
        begin
          p1 := Polygon[i - 1];
          p2 := Polygon[i];
          LineF(p1^, p2^, Color, True);
        end;

      p1 := Polygon[0];
      p2 := Polygon[L - 1];
      LineF(p1^, p2^, Color, True);
    end;
end;

procedure TMemoryRaster.FillPolygon(Polygon: T2DPolygonGraph; Color: TRColor);
  procedure Fill_Geo;
  var
    r: TRectV2;
{$IFDEF parallel}
{$IFDEF FPC}
    procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
    var
      p: PRColorArray;
      i: Integer;
      Pt: TVec2;
    begin
      p := ScanLine[pass];
      for i := 0 to width - 1 do
        begin
          Pt := Vec2(i, pass);
          if PointInRect(Pt, r) and Polygon.InHere(Pt) then
              p^[i] := Color;
        end;
    end;
{$ENDIF FPC}
{$ELSE parallel}
    procedure DoFor;
    var
      p: PRColorArray;
      pass, i: Integer;
      Pt: TVec2;
    begin
      for pass := 0 to height - 1 do
        begin
          p := ScanLine[pass];
          for i := 0 to width - 1 do
            begin
              Pt := Vec2(i, pass);
              if PointInRect(Pt, r) and Polygon.InHere(Pt) then
                  p^[i] := Color;
            end;
        end;
    end;
{$ENDIF parallel}

  begin
    r := Polygon.BoundRect;
{$IFDEF parallel}
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, height - 1);
{$ELSE}
    TParallel.for(0, height - 1, procedure(pass: Integer)
      var
        p: PRColorArray;
        i: Integer;
        Pt: TVec2;
      begin
        p := ScanLine[pass];
        for i := 0 to width - 1 do
          begin
            Pt := Vec2(i, pass);
            if PointInRect(Pt, r) and Polygon.InHere(Pt) then
                p^[i] := Color;
          end;
      end);
{$ENDIF FPC}
{$ELSE}
    DoFor();
{$ENDIF parallel}
  end;

begin
  Fill_Geo();
end;

procedure TMemoryRaster.DrawPolygon(Polygon: T2DPolygonGraph; Color: TRColor);
var
  i: Integer;
begin
  DrawPolygon(Polygon.Surround, Color);
  for i := 0 to Polygon.CollapsesCount - 1 do
      DrawPolygon(Polygon.Bands[i], Color);
end;

function TMemoryRaster.PixelAtBorder(const x, y: Integer; const BGColor, BorderColor: TRColor; const halfBorderSize: Integer; var detectColor: TRColor): Boolean;
var
  i, j: Integer;
  c: TRColor;
begin
  if Pixel[x, y] = BGColor then
    for j := y - halfBorderSize to y + halfBorderSize do
      if (j >= 0) and (j < height) then
        for i := x - halfBorderSize to x + halfBorderSize do
          if (i >= 0) and (i < width) then
            begin
              c := Pixel[i, j];
              if (c <> BGColor) and (c <> BorderColor) then
                begin
                  Result := True;
                  detectColor := c;
                  exit;
                end;
            end;
  Result := False;
end;

procedure TMemoryRaster.FillNoneBGColorBorder(BGColor, BorderColor: TRColor; BorderSize: Integer);
var
  halfBorderSize: Integer;

{$IFDEF parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    i: Integer;
    detectColor: TRColor;
  begin
    for i := 0 to width - 1 do
      if PixelAtBorder(i, pass, BGColor, BorderColor, halfBorderSize, detectColor) then
          Pixel[i, pass] := BorderColor;
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure DoFor;
  var
    i, pass: Integer;
    detectColor: TRColor;
  begin
    for pass := 0 to height - 1 do
      for i := 0 to width - 1 do
        if PixelAtBorder(i, pass, BGColor, BorderColor, halfBorderSize, detectColor) then
            Pixel[i, pass] := BorderColor;
  end;

{$ENDIF parallel}


begin
  if BGColor = BorderColor then
      exit;
  halfBorderSize := BorderSize shr 1;
  if halfBorderSize <= 0 then
      halfBorderSize := 1;

{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, height - 1);
{$ELSE FPC}
  TParallel.for(0, height - 1, procedure(pass: Integer)
    var
      i: Integer;
      detectColor: TRColor;
    begin
      for i := 0 to width - 1 do
        if PixelAtBorder(i, pass, BGColor, BorderColor, halfBorderSize, detectColor) then
            Pixel[i, pass] := BorderColor;
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor;
{$ENDIF parallel}
end;

function TMemoryRaster.TextSize(Text: SystemString; siz: TGeoFloat): TVec2;
begin
  Result := Vec2Mul(Font.TextSize(Text), siz / Font.FontSize)
end;

procedure TMemoryRaster.DrawText(Text: SystemString; x, y: Integer; RotateVec: TVec2; Angle, alpha, siz: TGeoFloat; TextColor: TRColor);
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

procedure TMemoryRaster.DrawText(Text: SystemString; x, y: Integer; siz: TGeoFloat; TextColor: TRColor);
begin
  DrawText(Text, x, y, Vec2(0.5, 0.5), 0, 1, siz, TextColor);
end;

procedure TMemoryRaster.ProjectionTo(Dst: TMemoryRaster; const sourRect, DestRect: TV2Rect4; const bilinear_sampling: Boolean; const alpha: Single);
begin
  Dst.Vertex.DrawRect(sourRect, DestRect, Self, bilinear_sampling, alpha);
end;

procedure TMemoryRaster.ProjectionTo(Dst: TMemoryRaster; const sourRect, DestRect: TRectV2; const bilinear_sampling: Boolean; const alpha: TGeoFloat);
begin
  ProjectionTo(Dst, TV2Rect4.Init(sourRect, 0), TV2Rect4.Init(DestRect, 0), bilinear_sampling, alpha);
end;

procedure TMemoryRaster.Projection(const DestRect: TV2Rect4; const Color: TRColor);
begin
  Vertex.DrawRect(DestRect, Color);
end;

procedure TMemoryRaster.Projection(sour: TMemoryRaster; const sourRect, DestRect: TV2Rect4; const bilinear_sampling: Boolean; const alpha: TGeoFloat);
begin
  Vertex.DrawRect(sourRect, DestRect, sour, bilinear_sampling, alpha);
end;

procedure TMemoryRaster.Projection(sour: TMemoryRaster; const sourRect, DestRect: TRectV2; const bilinear_sampling: Boolean; const alpha: TGeoFloat);
begin
  Projection(sour, TV2Rect4.Init(sourRect, 0), TV2Rect4.Init(DestRect, 0), bilinear_sampling, alpha);
end;

procedure TMemoryRaster.ProjectionPolygonTo(const sour_Polygon: TVec2List; Dst: TMemoryRaster; DestRect: TRectV2; const bilinear_sampling: Boolean; const alpha: TGeoFloat);
var
  r: TRectV2;
  nr: TMemoryRaster;

{$IFDEF parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    i: Integer;
  begin
    for i := 0 to width - 1 do
      if (PointInRect(i, pass, r)) and (sour_Polygon.InHere(Vec2(i, pass))) then
          nr[i, pass] := PixelLinear[i, pass];
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure DoFor;
  var
    pass, i: Integer;
  begin
    for pass := 0 to height - 1 do
      for i := 0 to width - 1 do
        if (PointInRect(i, pass, r)) and (sour_Polygon.InHere(Vec2(i, pass))) then
            nr[i, pass] := PixelLinear[i, pass];
  end;
{$ENDIF parallel}


begin
  nr := NewRaster();
  nr.SetSize(width, height, RColor(0, 0, 0, 0));
  r := sour_Polygon.BoundRect();

{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, height - 1);
{$ELSE FPC}
  TParallel.for(0, height - 1, procedure(pass: Integer)
    var
      i: Integer;
    begin
      for i := 0 to width - 1 do
        if (PointInRect(i, pass, r)) and (sour_Polygon.InHere(Vec2(i, pass))) then
            nr[i, pass] := PixelLinear[i, pass];
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor;
{$ENDIF parallel}
  nr.ProjectionTo(Dst, r, DestRect, bilinear_sampling, alpha);

  DisposeObject(nr);
end;

procedure TMemoryRaster.ProjectionPolygonTo(const sour_Polygon: T2DPolygonGraph; Dst: TMemoryRaster; DestRect: TRectV2; const bilinear_sampling: Boolean; const alpha: TGeoFloat);
var
  r: TRectV2;
  nr: TMemoryRaster;

{$IFDEF parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    i: Integer;
  begin
    for i := 0 to width - 1 do
      if (PointInRect(i, pass, r)) and (sour_Polygon.InHere(Vec2(i, pass))) then
          nr[i, pass] := PixelLinear[i, pass];
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure DoFor;
  var
    pass, i: Integer;
  begin
    for pass := 0 to height - 1 do
      for i := 0 to width - 1 do
        if (PointInRect(i, pass, r)) and (sour_Polygon.InHere(Vec2(i, pass))) then
            nr[i, pass] := PixelLinear[i, pass];
  end;
{$ENDIF parallel}


begin
  nr := NewRaster();
  nr.SetSize(width, height, RColor(0, 0, 0, 0));
  r := sour_Polygon.BoundRect;

{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, height - 1);
{$ELSE FPC}
  TParallel.for(0, height - 1, procedure(pass: Integer)
    var
      i: Integer;
    begin
      for i := 0 to width - 1 do
        if (PointInRect(i, pass, r)) and (sour_Polygon.InHere(Vec2(i, pass))) then
            nr[i, pass] := PixelLinear[i, pass];
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor;
{$ENDIF parallel}
  nr.ProjectionTo(Dst, r, DestRect, bilinear_sampling, alpha);
  DisposeObject(nr);
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

    stream.Read(hflag, 2);
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
      if (Header.biWidth <= 0) or (abs(Header.biHeight) <= 0) then
          exit;
      ReadyBits();

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

  stream.Read(hflag, 2);
  if hflag = $8D42 then
    begin
      m64 := TMemoryStream64.Create;
      DecompressStream(stream, m64);
      m64.Position := 0;
      LoadFromBmpStream(m64);
      DisposeObject(m64);
      exit;
    end
  else if hflag = $8D43 then
    begin
      m64 := TMemoryStream64.Create;
      DeflateDecompressStream(stream, m64);
      m64.Position := 0;
      LoadFromBmpStream(m64);
      DisposeObject(m64);
      exit;
    end
  else if hflag = $8D44 then
    begin
      m64 := TMemoryStream64.Create;
      BRRCDecompressStream(stream, m64);
      m64.Position := 0;
      LoadFromBmpStream(m64);
      DisposeObject(m64);
      exit;
    end
  else if hflag = $4D42 then
    begin
      stream.Position := bakPos;
      LoadFromBmpStream(stream);
    end
    // jls endian support
  else if (hflag = $D8FF) or (hflag = $8DFF) then
    begin
      stream.Read(hflag2, 2);
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
      exit;
    end
    // half yuv format
  else if hflag = $8D47 then
    begin
      HalfYUVToRasterization(stream, Self);
      exit;
    end
    // quart yuv format
  else if hflag = $8D48 then
    begin
      QuartYUVToRasterization(stream, Self);
      exit;
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

  ReadyBits();

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
  stream.Write(hflag, 2);

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
  stream.Write(hflag, 2);

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
  stream.Write(hflag, 2);

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
  stream.Write(hflag, 2);
  RasterizationToYV12(True, Self, stream);
end;

procedure TMemoryRaster.SaveToFastYV12Stream(stream: TCoreClassStream);
var
  hflag: Word;
begin
  hflag := $8D46; // yv12 format
  stream.Write(hflag, 2);
  RasterizationToYV12(False, Self, stream);
end;

procedure TMemoryRaster.SaveToHalfYUVStream(stream: TCoreClassStream);
var
  hflag: Word;
begin
  hflag := $8D47; // half yuv format
  stream.Write(hflag, 2);
  RasterizationToHalfYUV(True, Self, stream);
end;

procedure TMemoryRaster.SaveToFastHalfYUVStream(stream: TCoreClassStream);
var
  hflag: Word;
begin
  hflag := $8D47; // yu12 format
  stream.Write(hflag, 2);
  RasterizationToHalfYUV(False, Self, stream);
end;

procedure TMemoryRaster.SaveToQuartYUVStream(stream: TCoreClassStream);
var
  hflag: Word;
begin
  hflag := $8D48; // quart yuv format
  stream.Write(hflag, 2);
  RasterizationToQuartYUV(True, Self, stream);
end;

procedure TMemoryRaster.SaveToFastQuartYUVStream(stream: TCoreClassStream);
var
  hflag: Word;
begin
  hflag := $8D48; // quart yuv format
  stream.Write(hflag, 2);
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
  stream.Write(m64.Memory^, m64.Size);
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
  stream.Write(m64.Memory^, m64.Size);
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
  stream.Write(m64.Memory^, m64.Size);
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
  stream.Write(m64.Memory^, m64.Size);
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
  stream.Write(m64.Memory^, m64.Size);
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

procedure TByteRasterList.SaveToStream(stream: TCoreClassStream);
var
  d: TDataFrameEngine;
  i: Integer;
  br: TByteRaster;
  m64: TMemoryStream64;
begin
  d := TDataFrameEngine.Create;
  d.WriteInteger(Count);

  for i := 0 to Count - 1 do
    begin
      m64 := TMemoryStream64.CustomCreate(512 * 1024);
      br := Items[i];
      SaveByteRasterToStream(br, m64);
      d.WriteStream(m64);
      DisposeObject(m64);
    end;

  d.EncodeAsZLib(stream, True);

  DisposeObject(d);
end;

procedure TByteRasterList.LoadFromStream(stream: TCoreClassStream);
var
  d: TDataFrameEngine;
  c, i: Integer;
  br: TByteRaster;
  m64: TMemoryStream64;
begin
  Clear;
  d := TDataFrameEngine.Create;
  d.DecodeFrom(stream, True);

  c := d.Reader.ReadInteger;

  for i := 0 to c - 1 do
    begin
      m64 := TMemoryStream64.Create;
      d.Reader.ReadStream(m64);
      m64.Position := 0;
      LoadByteRasterFromStream(br, m64);
      Add(br);
      DisposeObject(m64);
    end;

  DisposeObject(d);
end;

constructor TRasterSerialized.Create(stream_: TCoreClassStream);
begin
  inherited Create;
  FStream := stream_;
  FAutoFreeStream := False;
  FCritical := TCritical.Create;
  FWriteList := TMemoryRasterList.Create;
  FReadList := TMemoryRasterList.Create;
end;

destructor TRasterSerialized.Destroy;
begin
  Clear();
  if FAutoFreeStream then
      DisposeObject(FStream);
  DisposeObject(FCritical);
  DisposeObject(FWriteList);
  DisposeObject(FReadList);
  inherited Destroy;
end;

function TRasterSerialized.Write(r: TMemoryRaster): Int64;
var
  h1, h2: TRasterSerializedHeader;
  p: Int64;
  i: Integer;
begin
  Result := 0;
  if (r = nil) or (r.empty) then
      exit;

  FCritical.Acquire;
  try
    h1.width := r.width;
    h1.height := r.height;
    h1.siz := r.width * r.height * 4;
    h1.UsedAGG := r.FAggNeed;

    p := FStream.Size;
    if r.FMemorySerializedPosition >= 0 then
      begin
        FStream.Position := r.FMemorySerializedPosition;
        // overwrite
        if FStream.Read(h2, SizeOf(TRasterSerializedHeader)) = SizeOf(TRasterSerializedHeader) then
          if (h2.width = h1.height) and (h2.height = h1.height) then
              p := r.FMemorySerializedPosition;
      end;

    FStream.Position := p;
    // serialized write
    if FStream.Write(h1, SizeOf(TRasterSerializedHeader)) = SizeOf(TRasterSerializedHeader) then
      if FStream.Write(r.FBits^[0], h1.siz) = h1.siz then
        begin
          r.CloseVertex;
          r.FreeAgg;
          System.FreeMemory(r.FBits);
          r.FBits := nil;
          r.FMemorySerializedPosition := p;
          Result := h1.siz;
        end;

    i := 0;
    while i < FWriteList.Count do
      begin
        if FWriteList[i] = r then
            FWriteList.Delete(i)
        else
            inc(i);
      end;
    FWriteList.Add(r);
  finally
      FCritical.Release;
  end;
end;

function TRasterSerialized.Read(r: TMemoryRaster): Int64;
var
  h: TRasterSerializedHeader;
  i: Integer;
begin
  Result := 0;
  if (r = nil) or (r.FMemorySerializedPosition < 0) then
      exit;

  r.CloseVertex;
  r.FreeAgg;

  FCritical.Acquire;
  try
    if (r.FMemorySerializedPosition >= 0) then
      begin
        FStream.Position := r.FMemorySerializedPosition;
        if FStream.Read(h, SizeOf(TRasterSerializedHeader)) = SizeOf(TRasterSerializedHeader) then
          if FStream.Position + h.siz <= FStream.Size then
            begin
              // recycle memory change
              if Assigned(r.FBits) and r.FFreeBits then
                  System.FreeMemory(r.FBits);

              // alloc serialized memory
              r.FBits := System.GetMemory(h.siz);
              r.FWidth := h.width;
              r.FHeight := h.height;

              // restore
              if FStream.Read(r.FBits^[0], h.siz) = h.siz then
                begin
                  r.FFreeBits := True;
                  if h.UsedAGG then
                      r.OpenAgg;
                  Result := h.siz;
                end;
            end;
      end;

    i := 0;
    while i < FReadList.Count do
      begin
        if FReadList[i] = r then
            FReadList.Delete(i)
        else
            inc(i);
      end;
    FReadList.Add(r);
  finally
      FCritical.Release;
  end;
end;

procedure TRasterSerialized.Remove(r: TMemoryRaster);
var
  i: Integer;
begin
  FCritical.Acquire;
  try
    i := 0;
    while i < FReadList.Count do
      begin
        if FReadList[i] = r then
            FReadList.Delete(i)
        else
            inc(i);
      end;

    i := 0;
    while i < FWriteList.Count do
      begin
        if FWriteList[i] = r then
            FWriteList.Delete(i)
        else
            inc(i);
      end;
  finally
      FCritical.Release;
  end;
end;

procedure TRasterSerialized.Clear;
begin
  while FReadList.Count > 0 do
      Remove(FReadList[0]);
  while FWriteList.Count > 0 do
      Remove(FWriteList[0]);
  FStream.Size := 0;
end;
