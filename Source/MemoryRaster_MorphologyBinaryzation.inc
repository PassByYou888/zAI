{ ****************************************************************************** }
{ * memory Rasterization                                                       * }
{ * by QQ 600585@qq.com                                                        * }
{ ****************************************************************************** }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }

procedure TMorphologyBinaryzationList.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DisposeObject(Items[i]);
  Clear;
end;

function TMorphologyBinaryzationLineHitAnalysis.AnalysisBox(const x1, y1, x2, y2: NativeInt; const PixelValue_: TBinaryzationValue): NativeInt;
begin
  FPixelSum := 0;
  FPixelValue := PixelValue_;
  FillBox(x1, y1, x2, y2);
  Result := FPixelSum;
end;

function TMorphologyBinaryzationLineHitAnalysis.AnalysisLine(const x1, y1, x2, y2: NativeInt; const PixelValue_: TBinaryzationValue): NativeInt;
begin
  FPixelSum := 0;
  FPixelValue := PixelValue_;
  Line(x1, y1, x2, y2);
  Result := FPixelSum;
end;

procedure TMorphologyBinaryzationLineHitAnalysis.Process(const vp: TMorphologyBinaryzationLineHitAnalysis_.PT_; const v: TBinaryzationValue);
begin
  if vp^ = FPixelValue then
      Inc(FPixelSum);
end;

function TMorphologyBinaryzation.GetPixel(const X, Y: Integer): TBinaryzationValue;
begin
  Result := FBits^[X + Y * FWidth];
end;

procedure TMorphologyBinaryzation.SetPixel(const X, Y: Integer; const Value: TBinaryzationValue);
begin
  FBits^[X + Y * FWidth] := Value;
end;

constructor TMorphologyBinaryzation.Create;
begin
  inherited Create;
  LocalParallel := True;
  FBits := nil;
  FWidth := 0;
  FHeight := 0;
end;

destructor TMorphologyBinaryzation.Destroy;
begin
  FreeBits();
  inherited Destroy;
end;

procedure TMorphologyBinaryzation.FreeBits;
begin
  if Assigned(FBits) then
    begin
      System.FreeMemory(FBits);
      FBits := nil;
    end;
  FWidth := 0;
  FHeight := 0;
end;

procedure TMorphologyBinaryzation.SetSize(const Width_, Height_: Integer);
begin
  if (Width_ = FWidth) and (Height_ = FHeight) and (FBits <> nil) then
      exit;

  FreeBits();
  FWidth := Width_;
  FHeight := Height_;
  FBits := System.GetMemory(DeltaStep(Width_ * Height_ * SizeOf(TBinaryzationValue), 4));
end;

procedure TMorphologyBinaryzation.SetSize(const Width_, Height_: Integer; const Value: TBinaryzationValue);
begin
  SetSize(Width_, Height_);
  FillValue(Value);
end;

procedure TMorphologyBinaryzation.SetSizeF(const Width_, Height_: TGeoFloat);
begin
  SetSize(Round(Width_), Round(Height_));
end;

procedure TMorphologyBinaryzation.SetSizeF(const Width_, Height_: TGeoFloat; const Value: TBinaryzationValue);
begin
  SetSize(Round(Width_), Round(Height_), Value);
end;

procedure TMorphologyBinaryzation.SetSizeR(const r: TRectV2);
begin
  SetSize(Round(RectWidth(r)), Round(RectHeight(r)));
end;

procedure TMorphologyBinaryzation.SetSizeR(const r: TRectV2; const Value: TBinaryzationValue);
begin
  SetSize(Round(RectWidth(r)), Round(RectHeight(r)), Value);
end;

procedure TMorphologyBinaryzation.SetConvolutionSize(const Width_, Height_: Integer; const Value: TBinaryzationValue);
var
  w, h: Integer;
begin
  // Convolution Calibrate
  w := Width_;
  if (w < 0) then
      w := 0;
  h := Height_;
  if (h < 0) then
      h := 0;
  // set size
  SetSize(w, h);
  FillValue(Value);
end;

procedure TMorphologyBinaryzation.FillValue(Value: TBinaryzationValue);
begin
  FillPtrByte(FBits, Width * Height * SizeOf(TBinaryzationValue), Byte(Value));
end;

procedure TMorphologyBinaryzation.FillRandomValue();
var
  i: Integer;
  rnd: TMT19937Random;
begin
  rnd := TMT19937Random.Create;
  for i := Width * Height - 1 downto 0 do
      FBits^[i] := Odd(rnd.Rand32(MaxInt));
  DisposeObject(rnd);
end;

procedure TMorphologyBinaryzation.FillValueFromPolygon(Polygon: TVec2List; InsideValue, OutsideValue: TBinaryzationValue);
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(Y: Integer);
  var
    X: Integer;
  begin
    for X := 0 to Width - 1 do
      begin
        if Polygon.InHere(Vec2(X, Y)) then
            Pixel[X, Y] := InsideValue
        else
            Pixel[X, Y] := OutsideValue;
      end;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    Y: Integer;
    X: Integer;
  begin
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        begin
          if Polygon.InHere(Vec2(X, Y)) then
              Pixel[X, Y] := InsideValue
          else
              Pixel[X, Y] := OutsideValue;
        end;
  end;
{$ENDIF Parallel}


begin
{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMorphologyBinaryzation.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMorphologyBinaryzation.Parallel and LocalParallel, 0, Height - 1, procedure(Y: Integer)
    var
      X: Integer;
    begin
      for X := 0 to Width - 1 do
        begin
          if Polygon.InHere(Vec2(X, Y)) then
              Pixel[X, Y] := InsideValue
          else
              Pixel[X, Y] := OutsideValue;
        end;
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
end;

function TMorphologyBinaryzation.ValueSum(Value: TBinaryzationValue): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := Width * Height - 1 downto 0 do
    if FBits^[i] = Value then
        Inc(Result);
end;

procedure TMorphologyBinaryzation.DrawLine(const x1, y1, x2, y2: Integer; const PixelValue_: TBinaryzationValue; const L: Boolean);
begin
  with TMorphologyBinaryzationDraw.Create(FBits, Width, Height, PixelValue_, L) do
    begin
      Line(x1, y1, x2, y2);
      Free;
    end;
end;

procedure TMorphologyBinaryzation.FillBox(const x1, y1, x2, y2: Integer; const PixelValue_: TBinaryzationValue);
begin
  with TMorphologyBinaryzationDraw.Create(FBits, Width, Height, PixelValue_, True) do
    begin
      FillBox(x1, y1, x2, y2);
      Free;
    end;
end;

function TMorphologyBinaryzation.LineHitSum(const x1, y1, x2, y2: Integer; const PixelValue_: TBinaryzationValue; const L: Boolean): Integer;
begin
  with TMorphologyBinaryzationLineHitAnalysis.Create(FBits, Width, Height, PixelValue_, L) do
    begin
      Result := AnalysisLine(x1, y1, x2, y2, PixelValue_);
      Free;
    end;
end;

function TMorphologyBinaryzation.BoxHitSum(const x1, y1, x2, y2: Integer; const PixelValue_: TBinaryzationValue): Integer;
begin
  with TMorphologyBinaryzationLineHitAnalysis.Create(FBits, Width, Height, PixelValue_, True) do
    begin
      Result := AnalysisBox(x1, y1, x2, y2, PixelValue_);
      Free;
    end;
end;

function TMorphologyBinaryzation.BoxHitSum(const r: TRect; const PixelValue_: TBinaryzationValue): Integer;
begin
  Result := BoxHitSum(r.Left, r.Top, r.Right, r.Bottom, PixelValue_);
end;

function TMorphologyBinaryzation.BoxHitSum(const r: TRectV2; const PixelValue_: TBinaryzationValue): Integer;
begin
  Result := BoxHitSum(Rect2Rect(r), PixelValue_);
end;

function TMorphologyBinaryzation.BuildHoughLine(const MaxAngle_, AlphaStep_: TGeoFloat; const BestLinesCount_: Integer): THoughLineArry;
begin
  Result := BuildBinHoughLine(MaxAngle_, AlphaStep_, BestLinesCount_, Self);
end;

procedure TMorphologyBinaryzation.ProjectionTo(SourMorph_, DestMorph_: TMorphologyPixel; Dst: TMorphologyBinaryzation; sourRect, DestRect: TV2Rect4; bilinear_sampling: Boolean; alpha: TGeoFloat);
var
  sour_r, dest_r: TMemoryRaster;
  tmp: TMorphologyBinaryzation;
begin
  sour_r := BuildViewer(SourMorph_);
  dest_r := Dst.BuildViewer(DestMorph_);
  sour_r.ProjectionTo(dest_r, sourRect, DestRect, bilinear_sampling, alpha);
  with dest_r.BuildMorphomatics(DestMorph_) do
    begin
      tmp := Binarization(0.5);
      Free;
    end;
  Dst.SwapData(tmp);
  DisposeObject(tmp);
  DisposeObject(sour_r);
  DisposeObject(dest_r);
end;

procedure TMorphologyBinaryzation.ProjectionTo(SourMorph_, DestMorph_: TMorphologyPixel; Dst: TMorphologyBinaryzation; sourRect, DestRect: TRectV2; bilinear_sampling: Boolean; alpha: TGeoFloat);
begin
  ProjectionTo(SourMorph_, DestMorph_, Dst, TV2Rect4.Init(sourRect, 0), TV2Rect4.Init(DestRect, 0), bilinear_sampling, alpha);
end;

procedure TMorphologyBinaryzation.Projection(SourMorph_, DestMorph_: TMorphologyPixel; DestRect: TV2Rect4; Value: TBinaryzationValue);
var
  sour_r: TMemoryRaster;
  c: TRColor;
  tmp: TMorphologyBinaryzation;
begin
  sour_r := BuildViewer(SourMorph_);
  MorphToRColor(DestMorph_, if_(Value, 1.0, 0), c);
  sour_r.Projection(DestRect, c);
  with sour_r.BuildMorphomatics(DestMorph_) do
    begin
      tmp := Binarization(0.5);
      Free;
    end;
  SwapData(tmp);
  DisposeObject(tmp);
  DisposeObject(sour_r);
end;

procedure TMorphologyBinaryzation.ProjectionTo(Dst: TMorphologyBinaryzation; sourRect, DestRect: TV2Rect4; bilinear_sampling: Boolean; alpha: TGeoFloat);
begin
  ProjectionTo(mpGrayscale, mpGrayscale, Dst, sourRect, DestRect, bilinear_sampling, alpha);
end;

procedure TMorphologyBinaryzation.ProjectionTo(Dst: TMorphologyBinaryzation; sourRect, DestRect: TRectV2; bilinear_sampling: Boolean; alpha: TGeoFloat);
begin
  ProjectionTo(mpGrayscale, mpGrayscale, Dst, sourRect, DestRect, bilinear_sampling, alpha);
end;

procedure TMorphologyBinaryzation.Projection(DestRect: TV2Rect4; Value: TBinaryzationValue);
begin
  Projection(mpGrayscale, mpGrayscale, DestRect, Value);
end;

procedure TMorphologyBinaryzation.IfThenSet(IfValue: TBinaryzationValue; dest: TMemoryRaster; destValue: TRColor);
var
  i, j: Integer;
begin
  if Width * Height <> dest.Width * dest.Height then
      RaiseInfo('size must be equal %d * %d', [Width, Height]);
  for j := 0 to Height - 1 do
    for i := 0 to Width - 1 do
      if Pixel[i, j] = IfValue then
          dest[i, j] := destValue;
end;

function TMorphologyBinaryzation.Clone: TMorphologyBinaryzation;
begin
  Result := TMorphologyBinaryzation.Create;
  Result.LocalParallel := LocalParallel;
  Result.Assign(Self);
end;

procedure TMorphologyBinaryzation.Assign(sour: TMorphologyBinaryzation);
begin
  SetSize(sour.Width, sour.Height);
  CopyPtr(@sour.FBits^[0], @FBits^[0], Width * Height * SizeOf(TBinaryzationValue));
end;

procedure TMorphologyBinaryzation.SaveToStream(stream: TCoreClassStream);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.WriteInt32(Width);
  m64.WriteInt32(Height);
  m64.WritePtr(@FBits^[0], Width * Height * SizeOf(TBinaryzationValue));
  m64.Position := 0;
  FastCompressStream(m64, stream);
  DisposeObject(m64);
end;

procedure TMorphologyBinaryzation.LoadFromStream(stream: TCoreClassStream);
var
  m64: TMemoryStream64;
  w, h: Integer;
begin
  m64 := TMemoryStream64.Create;
  DecompressStream(stream, m64);
  m64.Position := 0;
  w := m64.ReadInt32;
  h := m64.ReadInt32;
  SetSize(w, h);
  m64.ReadPtr(@FBits^[0], Width * Height * SizeOf(TBinaryzationValue));
  DisposeObject(m64);
end;

procedure TMorphologyBinaryzation.SwapData(dest: TMorphologyBinaryzation);
var
  bak_Bits: PBinaryzationBits;
  bak_Width, bak_Height: Integer;
begin
  bak_Bits := FBits;
  bak_Width := FWidth;
  bak_Height := FHeight;

  FBits := dest.FBits;
  FWidth := dest.FWidth;
  FHeight := dest.FHeight;

  dest.FBits := bak_Bits;
  dest.FWidth := bak_Width;
  dest.FHeight := bak_Height;
end;

procedure TMorphologyBinaryzation.Invert;
var
  i: Integer;
begin
  for i := Width * Height - 1 downto 0 do
      FBits^[i] := not FBits^[i];
end;

function TMorphologyBinaryzation.BuildMorphologySegmentation(): TMorphologySegmentation;
begin
  Result := TMorphologySegmentation.Create;
  Result.BuildSegmentation(Self);
end;

function TMorphologyBinaryzation.BuildMorphomatics(): TMorphomatics;
var
  i: Integer;
begin
  Result := TMorphomatics.Create;
  Result.LocalParallel := LocalParallel;
  Result.SetSize(Width, Height);
  for i := Width * Height - 1 downto 0 do
      Result.FBits^[i] := if_(FBits^[i], 1.0, 0);
end;

procedure TMorphologyBinaryzation.DrawTo(raster: TMemoryRaster);
begin
  raster.DrawBinaryzation(Self);
end;

procedure TMorphologyBinaryzation.DrawTo(MorphPix_: TMorphologyPixel; raster: TMemoryRaster);
begin
  raster.DrawBinaryzation(MorphPix_, Self);
end;

function TMorphologyBinaryzation.BuildViewer(): TMemoryRaster;
begin
  Result := NewRaster();
  Result.LocalParallel := LocalParallel;
  Result.SetSize(Width, Height);
  DrawTo(Result);
end;

function TMorphologyBinaryzation.BuildViewer(MorphPix_: TMorphologyPixel): TMemoryRaster;
begin
  Result := NewRaster();
  Result.LocalParallel := LocalParallel;
  Result.SetSize(Width, Height, RColor(0, 0, 0));
  DrawTo(MorphPix_, Result);
end;

procedure TMorphologyBinaryzation.BuildViewerFile(filename_: SystemString);
begin
  with BuildViewer() do
    begin
      SaveToFile(filename_);
      Free;
    end;
end;

procedure TMorphologyBinaryzation.BuildViewerFile(MorphPix_: TMorphologyPixel; filename_: SystemString);
begin
  with BuildViewer(MorphPix_) do
    begin
      SaveToFile(filename_);
      Free;
    end;
end;

function TMorphologyBinaryzation.ConvexHull(): TVec2List;
var
  i, j: Integer;
  found: Boolean;
begin
  Result := TVec2List.Create;
  for j := 0 to Height - 1 do
    begin
      found := False;
      for i := 0 to Width - 1 do
        if found then
          begin
            if not Pixel[i, j] then
              begin
                Result.Add(i, j);
                found := False;
              end;
          end
        else if Pixel[i, j] then
          begin
            Result.Add(i, j);
            found := True;
          end;
    end;
end;

function TMorphologyBinaryzation.BoundsRectV2(const Value: TBinaryzationValue; var Sum_: Integer): TRectV2;
var
  color_inited_: Boolean;
  i, j: Integer;
begin
  color_inited_ := False;
  Result := NullRectV2;
  Sum_ := 0;
  for j := 0 to Height - 1 do
    for i := 0 to Width - 1 do
      if Pixel[i, j] = Value then
        begin
          Inc(Sum_);
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

function TMorphologyBinaryzation.BoundsRectV2(const Value: TBinaryzationValue): TRectV2;
var
  color_inited_: Boolean;
  i, j: Integer;
begin
  color_inited_ := False;
  Result := NullRectV2;
  for j := 0 to Height - 1 do
    for i := 0 to Width - 1 do
      if Pixel[i, j] = Value then
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

function TMorphologyBinaryzation.BoundsRect(const Value: TBinaryzationValue; var Sum_: Integer): TRect;
var
  color_inited_: Boolean;
  i, j: Integer;
begin
  color_inited_ := False;
  Result := Rect(0, 0, 0, 0);
  Sum_ := 0;
  for j := 0 to Height - 1 do
    for i := 0 to Width - 1 do
      if Pixel[i, j] = Value then
        begin
          Inc(Sum_);
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

function TMorphologyBinaryzation.BoundsRect(const Value: TBinaryzationValue): TRect;
var
  color_inited_: Boolean;
  i, j: Integer;
begin
  color_inited_ := False;
  Result := Rect(0, 0, 0, 0);
  for j := 0 to Height - 1 do
    for i := 0 to Width - 1 do
      if Pixel[i, j] = Value then
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

function TMorphologyBinaryzation.Width0: Integer;
begin
  if FWidth > 0 then
      Result := FWidth - 1
  else
      Result := 0;
end;

function TMorphologyBinaryzation.Height0: Integer;
begin
  if FHeight > 0 then
      Result := FHeight - 1
  else
      Result := 0;
end;

function TMorphologyBinaryzation.SizeOfPoint: TPoint;
begin
  Result := Point(Width, Height);
end;

function TMorphologyBinaryzation.SizeOf2DPoint: TVec2;
begin
  Result := Vec2(Width, Height);
end;

function TMorphologyBinaryzation.Size2D: TVec2;
begin
  Result := Vec2(Width, Height);
end;

function TMorphologyBinaryzation.Size0: TVec2;
begin
  Result := Vec2(Width0, Height0);
end;

function TMorphologyBinaryzation.BoundsRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

function TMorphologyBinaryzation.BoundsRect0: TRect;
begin
  Result := Rect(0, 0, Width0, Height0);
end;

function TMorphologyBinaryzation.BoundsRectV2: TRectV2;
begin
  Result := RectV2(0, 0, Width, Height);
end;

function TMorphologyBinaryzation.BoundsRectV20: TRectV2;
begin
  Result := RectV2(0, 0, Width0, Height0);
end;

function TMorphologyBinaryzation.BoundsV2Rect4: TV2Rect4;
begin
  Result := TV2Rect4.Init(BoundsRectV2);
end;

function TMorphologyBinaryzation.BoundsV2Rect40: TV2Rect4;
begin
  Result := TV2Rect4.Init(BoundsRectV20);
end;

function TMorphologyBinaryzation.Centroid: TVec2;
begin
  Result := RectCentre(BoundsRectV2);
end;

function TMorphologyBinaryzation.Centre: TVec2;
begin
  Result := RectCentre(BoundsRectV2);
end;

function TMorphologyBinaryzation.InHere(const X, Y: Integer): Boolean;
begin
  Result := PointInRect(X, Y, 0, 0, Width0, Height0);
end;

procedure TMorphologyBinaryzation.Dilatation(ConvolutionKernel, Output: TMorphologyBinaryzation);
var
  kH, kW, kOffsetY, kModY, kOffsetX, kModX: Integer;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(sY: Integer);
  var
    sX, aY, aX, kY, kX: Integer;
  begin
    for sX := 0 to Width - 1 do
      if Pixel[sX, sY] then
        begin
          kY := 0;
          for aY := sY - kOffsetY to kH - kOffsetY + sY + kModY do
            begin
              kX := 0;
              for aX := sX - kOffsetX to kW - kOffsetX + sX + kModX do
                begin
                  if (aY >= 0) and (aY < Height) and (aX >= 0) and (aX < Width) and (not Output[aX, aY]) and (Pixel[aX, aY] or ConvolutionKernel[kX, kY]) then
                      Output[aX, aY] := True;
                  Inc(kX);
                end;
              Inc(kY);
            end;
        end;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    sY: Integer;
    sX, aY, aX, kY, kX: Integer;
  begin
    for sY := 0 to Height - 1 do
      for sX := 0 to Width - 1 do
        if Pixel[sX, sY] then
          begin
            kY := 0;
            for aY := sY - kOffsetY to kH - kOffsetY + sY + kModY do
              begin
                kX := 0;
                for aX := sX - kOffsetX to kW - kOffsetX + sX + kModX do
                  begin
                    if (aY >= 0) and (aY < Height) and (aX >= 0) and (aX < Width) and (not Output[aX, aY]) and (Pixel[aX, aY] or ConvolutionKernel[kX, kY]) then
                        Output[aX, aY] := True;
                    Inc(kX);
                  end;
                Inc(kY);
              end;
          end;
  end;
{$ENDIF Parallel}


begin
  kH := ConvolutionKernel.Height;
  kW := ConvolutionKernel.Width;
  kOffsetY := kH div 2;
  kModY := if_(not Odd(kH), 1, 0);
  kOffsetX := kW div 2;
  kModX := if_(not Odd(kW), 1, 0);
  dec(kH);
  dec(kW);

  Output.SetSize(Width, Height, False);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMorphologyBinaryzation.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMorphologyBinaryzation.Parallel and LocalParallel, 0, Height - 1, procedure(sY: Integer)
    var
      sX, aY, aX, kY, kX: Integer;
    begin
      for sX := 0 to Width - 1 do
        if Pixel[sX, sY] then
          begin
            kY := 0;
            for aY := sY - kOffsetY to kH - kOffsetY + sY + kModY do
              begin
                kX := 0;
                for aX := sX - kOffsetX to kW - kOffsetX + sX + kModX do
                  begin
                    if (aY >= 0) and (aY < Height) and (aX >= 0) and (aX < Width) and (not Output[aX, aY]) and (Pixel[aX, aY] or ConvolutionKernel[kX, kY]) then
                        Output[aX, aY] := True;
                    Inc(kX);
                  end;
                Inc(kY);
              end;
          end;
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
end;

procedure TMorphologyBinaryzation.Erosion(ConvolutionKernel, Output: TMorphologyBinaryzation);
var
  kH, kW, kOffsetY, kModY, kOffsetX, kModX: Integer;
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(sY: Integer);
  var
    sX, aY, aX, kY, kX: Integer;
    ConvolutionState: Boolean;
  begin
    for sX := 0 to Width - 1 do
      begin
        ConvolutionState := True;
        kY := 0;
        for aY := sY - kOffsetY to kH - kOffsetY + sY + kModY do
          begin
            kX := 0;
            for aX := sX - kOffsetX to kW - kOffsetX + sX + kModX do
              begin
                if (aY >= 0) and (aY < Height) and (aX >= 0) and (aX < Width) then
                  begin
                    if ConvolutionKernel[kX, kY] then
                        ConvolutionState := ConvolutionState and Pixel[aX, aY];
                  end
                else
                    ConvolutionState := False;
                Inc(kX);
              end;
            Inc(kY);
          end;
        Output[sX, sY] := ConvolutionState;
      end;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    sY: Integer;
    sX, aY, aX, kY, kX: Integer;
    ConvolutionState: Boolean;
  begin
    for sY := 0 to Height - 1 do
      for sX := 0 to Width - 1 do
        begin
          ConvolutionState := True;
          kY := 0;
          for aY := sY - kOffsetY to kH - kOffsetY + sY + kModY do
            begin
              kX := 0;
              for aX := sX - kOffsetX to kW - kOffsetX + sX + kModX do
                begin
                  if (aY >= 0) and (aY < Height) and (aX >= 0) and (aX < Width) then
                    begin
                      if ConvolutionKernel[kX, kY] then
                          ConvolutionState := ConvolutionState and Pixel[aX, aY];
                    end
                  else
                      ConvolutionState := False;
                  Inc(kX);
                end;
              Inc(kY);
            end;
          Output[sX, sY] := ConvolutionState;
        end;
  end;
{$ENDIF Parallel}


begin
  kH := ConvolutionKernel.Height;
  kW := ConvolutionKernel.Width;
  kOffsetY := kH div 2;
  kModY := if_(not Odd(kH), 1, 0);
  kOffsetX := kW div 2;
  kModX := if_(not Odd(kW), 1, 0);
  dec(kH);
  dec(kW);

  Output.SetSize(Width, Height, False);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMorphologyBinaryzation.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMorphologyBinaryzation.Parallel and LocalParallel, 0, Height - 1, procedure(sY: Integer)
    var
      sX, aY, aX, kY, kX: Integer;
      ConvolutionState: Boolean;
    begin
      for sX := 0 to Width - 1 do
        begin
          ConvolutionState := True;
          kY := 0;
          for aY := sY - kOffsetY to kH - kOffsetY + sY + kModY do
            begin
              kX := 0;
              for aX := sX - kOffsetX to kW - kOffsetX + sX + kModX do
                begin
                  if (aY >= 0) and (aY < Height) and (aX >= 0) and (aX < Width) then
                    begin
                      if ConvolutionKernel[kX, kY] then
                          ConvolutionState := ConvolutionState and Pixel[aX, aY];
                    end
                  else
                      ConvolutionState := False;
                  Inc(kX);
                end;
              Inc(kY);
            end;
          Output[sX, sY] := ConvolutionState;
        end;
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
end;

procedure TMorphologyBinaryzation.Opening(ConvolutionKernel, Output: TMorphologyBinaryzation);
begin
  Erosion(ConvolutionKernel, Output);
  Output.Dilatation(ConvolutionKernel);
end;

procedure TMorphologyBinaryzation.Closing(ConvolutionKernel, Output: TMorphologyBinaryzation);
begin
  Dilatation(ConvolutionKernel, Output);
  Output.Erosion(ConvolutionKernel);
end;

procedure TMorphologyBinaryzation.OpeningAndClosing(ConvolutionKernel, Output: TMorphologyBinaryzation);
begin
  Opening(ConvolutionKernel, Output);
  Output.Closing(ConvolutionKernel);
end;

procedure TMorphologyBinaryzation.ClosingAndOpening(ConvolutionKernel, Output: TMorphologyBinaryzation);
begin
  Output.Closing(ConvolutionKernel);
  Opening(ConvolutionKernel, Output);
end;

procedure TMorphologyBinaryzation.Skeleton(ConvolutionKernel, Output: TMorphologyBinaryzation);
var
  tmp1, tmp2, tmp3, tmpFinal: TMorphologyBinaryzation;
begin
  tmp1 := Clone();
  tmp2 := TMorphologyBinaryzation.Create;
  tmp3 := TMorphologyBinaryzation.Create;
  tmpFinal := TMorphologyBinaryzation.Create;
  tmpFinal.SetSize(Width, Height, False);

  while True do
    begin
      // opening compute
      tmp1.Erosion(ConvolutionKernel, tmp2);
      tmp2.Dilatation(ConvolutionKernel, tmp3);
      tmp3.XOR_(tmp1);
      if CompareMemory(@tmp3.FBits^[0], @tmp1.FBits^[0], Width * Height * SizeOf(TBinaryzationValue)) then
          break;
      // combine
      tmpFinal.OR_(tmp3);
      // erosion optimized
      tmp1.SwapData(tmp2);
    end;

  Output.SwapData(tmpFinal);
  DisposeObject(tmp1);
  DisposeObject(tmp2);
  DisposeObject(tmp3);
  DisposeObject(tmpFinal);
end;

procedure TMorphologyBinaryzation.Dilatation(ConvolutionKernel: TMorphologyBinaryzation);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  Dilatation(ConvolutionKernel, tmp);
  SwapData(tmp);
  DisposeObject(tmp);
end;

procedure TMorphologyBinaryzation.Erosion(ConvolutionKernel: TMorphologyBinaryzation);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  Erosion(ConvolutionKernel, tmp);
  SwapData(tmp);
  DisposeObject(tmp);
end;

procedure TMorphologyBinaryzation.Opening(ConvolutionKernel: TMorphologyBinaryzation);
begin
  Erosion(ConvolutionKernel);
  Dilatation(ConvolutionKernel);
end;

procedure TMorphologyBinaryzation.Closing(ConvolutionKernel: TMorphologyBinaryzation);
begin
  Dilatation(ConvolutionKernel);
  Erosion(ConvolutionKernel);
end;

procedure TMorphologyBinaryzation.OpeningAndClosing(ConvolutionKernel: TMorphologyBinaryzation);
begin
  Opening(ConvolutionKernel);
  Closing(ConvolutionKernel);
end;

procedure TMorphologyBinaryzation.ClosingAndOpening(ConvolutionKernel: TMorphologyBinaryzation);
begin
  Closing(ConvolutionKernel);
  Opening(ConvolutionKernel);
end;

procedure TMorphologyBinaryzation.Skeleton(ConvolutionKernel: TMorphologyBinaryzation);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  Skeleton(ConvolutionKernel, tmp);
  SwapData(tmp);
  DisposeObject(tmp);
end;

procedure TMorphologyBinaryzation.Dilatation(const ConvolutionSizeX, ConvolutionSizeY: Integer; Output: TMorphologyBinaryzation);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  Dilatation(tmp, Output);
  DisposeObject(tmp);
end;

procedure TMorphologyBinaryzation.Erosion(const ConvolutionSizeX, ConvolutionSizeY: Integer; Output: TMorphologyBinaryzation);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  Erosion(tmp, Output);
  DisposeObject(tmp);
end;

procedure TMorphologyBinaryzation.Opening(const ConvolutionSizeX, ConvolutionSizeY: Integer; Output: TMorphologyBinaryzation);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  Opening(tmp, Output);
  DisposeObject(tmp);
end;

procedure TMorphologyBinaryzation.Closing(const ConvolutionSizeX, ConvolutionSizeY: Integer; Output: TMorphologyBinaryzation);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  Closing(tmp, Output);
  DisposeObject(tmp);
end;

procedure TMorphologyBinaryzation.OpeningAndClosing(const ConvolutionSizeX, ConvolutionSizeY: Integer; Output: TMorphologyBinaryzation);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  OpeningAndClosing(tmp, Output);
  DisposeObject(tmp);
end;

procedure TMorphologyBinaryzation.ClosingAndOpening(const ConvolutionSizeX, ConvolutionSizeY: Integer; Output: TMorphologyBinaryzation);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  ClosingAndOpening(tmp, Output);
  DisposeObject(tmp);
end;

procedure TMorphologyBinaryzation.Skeleton(const ConvolutionSizeX, ConvolutionSizeY: Integer; Output: TMorphologyBinaryzation);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  Skeleton(tmp, Output);
  DisposeObject(tmp);
end;

procedure TMorphologyBinaryzation.Dilatation(const ConvolutionSizeX, ConvolutionSizeY: Integer);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  Dilatation(tmp);
  DisposeObject(tmp);
end;

procedure TMorphologyBinaryzation.Erosion(const ConvolutionSizeX, ConvolutionSizeY: Integer);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  Erosion(tmp);
  DisposeObject(tmp);
end;

procedure TMorphologyBinaryzation.Opening(const ConvolutionSizeX, ConvolutionSizeY: Integer);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  Opening(tmp);
  DisposeObject(tmp);
end;

procedure TMorphologyBinaryzation.Closing(const ConvolutionSizeX, ConvolutionSizeY: Integer);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  Closing(tmp);
  DisposeObject(tmp);
end;

procedure TMorphologyBinaryzation.OpeningAndClosing(const ConvolutionSizeX, ConvolutionSizeY: Integer);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  OpeningAndClosing(tmp);
  DisposeObject(tmp);
end;

procedure TMorphologyBinaryzation.ClosingAndOpening(const ConvolutionSizeX, ConvolutionSizeY: Integer);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  ClosingAndOpening(tmp);
  DisposeObject(tmp);
end;

procedure TMorphologyBinaryzation.Skeleton(const ConvolutionSizeX, ConvolutionSizeY: Integer);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  Skeleton(tmp);
  DisposeObject(tmp);
end;

procedure TMorphologyBinaryzation.OR_(Source, Output: TMorphologyBinaryzation);
var
  i: Integer;
begin
  if Source.Width * Source.Height <> Width * Height then
      RaiseInfo('size must be equal %d * %d', [Width, Height]);
  if Output.Width * Output.Height <> Width * Height then
      RaiseInfo('output size must be equal %d * %d', [Width, Height]);
  for i := Width * Height - 1 downto 0 do
      Output.FBits^[i] := FBits^[i] or Source.FBits^[i];
end;

procedure TMorphologyBinaryzation.AND_(Source, Output: TMorphologyBinaryzation);
var
  i: Integer;
begin
  if Source.Width * Source.Height <> Width * Height then
      RaiseInfo('size must be equal %d * %d', [Width, Height]);
  if Output.Width * Output.Height <> Width * Height then
      RaiseInfo('output size must be equal %d * %d', [Width, Height]);
  for i := Width * Height - 1 downto 0 do
      Output.FBits^[i] := FBits^[i] and Source.FBits^[i];
end;

procedure TMorphologyBinaryzation.XOR_(Source, Output: TMorphologyBinaryzation);
var
  i: Integer;
begin
  if Source.Width * Source.Height <> Width * Height then
      RaiseInfo('size must be equal %d * %d', [Width, Height]);
  if Output.Width * Output.Height <> Width * Height then
      RaiseInfo('output size must be equal %d * %d', [Width, Height]);
  for i := Width * Height - 1 downto 0 do
      Output.FBits^[i] := FBits^[i] xor Source.FBits^[i];
end;

procedure TMorphologyBinaryzation.OR_(Source: TMorphologyBinaryzation);
var
  i: Integer;
begin
  if Source.Width * Source.Height <> Width * Height then
      RaiseInfo('size must be equal %d * %d', [Width, Height]);

  for i := Width * Height - 1 downto 0 do
      FBits^[i] := FBits^[i] or Source.FBits^[i];
end;

procedure TMorphologyBinaryzation.AND_(Source: TMorphologyBinaryzation);
var
  i: Integer;
begin
  if Source.Width * Source.Height <> Width * Height then
      RaiseInfo('size must be equal %d * %d', [Width, Height]);

  for i := Width * Height - 1 downto 0 do
      FBits^[i] := FBits^[i] and Source.FBits^[i];
end;

procedure TMorphologyBinaryzation.XOR_(Source: TMorphologyBinaryzation);
var
  i: Integer;
begin
  if Source.Width * Source.Height <> Width * Height then
      RaiseInfo('size must be equal %d * %d', [Width, Height]);

  for i := Width * Height - 1 downto 0 do
      FBits^[i] := FBits^[i] xor Source.FBits^[i];
end;

procedure TMorphologyBinaryzation.Process(Operation_: TBinaryzationOperation; Data: TMorphologyBinaryzation);
begin
  if (Operation_ <> boNone) and (Data <> nil) then
    case Operation_ of
      boDilatation: Dilatation(Data);
      boErosion: Erosion(Data);
      boOpening: Opening(Data);
      boClosing: Closing(Data);
      boOpeningAndClosing: OpeningAndClosing(Data);
      boClosingAndOpening: ClosingAndOpening(Data);
      boOR: OR_(Data);
      boAND: AND_(Data);
      boXOR: XOR_(Data);
    end;
end;

procedure TMorphologyBinaryzation.Print;
var
  i, j: Integer;
begin
  DoStatusNoLn;
  for j := 0 to Height - 1 do
    begin
      for i := 0 to Width - 1 do
        if Pixel[i, j] then
            DoStatusNoLn('*')
        else
            DoStatusNoLn('-');
      DoStatusNoLn;
    end;
  DoStatus('');
end;

class procedure TMorphologyBinaryzation.Test;
var
  k, ori, tmp: TMorphologyBinaryzation;
  m64: TMemoryStream64;
  md5_1, md5_2: TMD5;
  tk: TTimeTick;
begin
  with TMorphologyBinaryzation.Create do
    begin
      SetSize(100, 100, False);
      DrawLine(0, 0, 100, 100, True, True);
      DrawLine(0, 0, 100, 0, True, True);
      DrawLine(0, 0, 0, 100, True, True);
      DoStatus('Binaryzation LineHitSum: %d', [LineHitSum(0, 0, 100, 100, True, True)]);
      Free;
    end;

  DoStatus('kernel');
  k := TMorphologyBinaryzation.Create;
  k.SetSize(3, 3, True);
  k.Print;

  DoStatus('data source');
  ori := TMorphologyBinaryzation.Create;
  ori.SetSize(15, 10, False);

  ori[3, 3] := True;
  ori[3, 4] := True;
  ori[3, 5] := True;

  ori[4, 3] := True;
  ori[4, 4] := True;
  ori[4, 5] := True;

  ori[5, 3] := True;
  ori[5, 4] := True;
  ori[5, 5] := True;

  ori[6, 5] := True;
  ori[7, 3] := True;
  ori[7, 4] := True;

  ori[7, 5] := True;

  ori[8, 3] := True;
  ori[8, 4] := True;
  ori[8, 5] := True;

  ori[9, 2] := True;

  ori[9, 3] := True;
  ori[9, 4] := True;
  ori[9, 5] := True;

  ori[10, 3] := True;
  ori[10, 4] := True;
  ori[10, 5] := True;

  ori.Print;

  tmp := TMorphologyBinaryzation.Create;
  DoStatus('Dilatation');
  tmp.Assign(ori);
  tmp.Dilatation(k);
  tmp.Print;

  DoStatus('Erosion');
  tmp.Assign(ori);
  tmp.Erosion(k);
  tmp.Print;

  DoStatus('opening');
  tmp.Assign(ori);
  tmp.Opening(k);
  tmp.Print;

  DoStatus('closing');
  tmp.Assign(ori);
  tmp.Closing(k);
  tmp.Print;

  DoStatus('OpeningAndClosing');
  tmp.Assign(ori);
  tmp.OpeningAndClosing(k);
  tmp.Print;

  DoStatus('ClosingAndOpening');
  tmp.Assign(ori);
  tmp.ClosingAndOpening(k);
  tmp.Print;

  DoStatus('Skeleton');
  tmp.Assign(ori);
  tmp.Skeleton(k);
  tmp.Print;

  DisposeObject(ori);
  DisposeObject(tmp);
  DisposeObject(k);

  exit;

  DoStatus('LargeScale Binaryzation peformance test');
  k := TMorphologyBinaryzation.Create;
  k.SetSize(16 * 2, 16 * 2 + 1);
  k.FillRandomValue;
  ori := TMorphologyBinaryzation.Create;
  ori.SetSize(1024, 1024);
  ori.FillRandomValue;
  tmp := TMorphologyBinaryzation.Create;

  DoStatusNoLn('TESTING Binaryzation SAVESTREAM..');
  m64 := TMemoryStream64.Create;
  ori.SaveToStream(m64);
  md5_1 := umlStreamMD5(m64);
  m64.Position := 0;
  DoStatusNoLn('TESTING Binaryzation LOADSTREAM..');
  tmp.LoadFromStream(m64);
  m64.Clear;
  tmp.SaveToStream(m64);
  md5_2 := umlStreamMD5(m64);
  if umlMD5Compare(md5_1, md5_2) then
      DoStatus('STREAM Binaryzation VERIFY OK')
  else
      DoStatus('STREAM Binaryzation VERIFY FAILED');
  DisposeObject(m64);
  DoStatusNoLn;

  tmp.Assign(ori);
  tk := GetTimeTick;
  tmp.Dilatation(k);
  DoStatus('Dilatation %dms', [GetTimeTick - tk]);

  tmp.Assign(ori);
  tk := GetTimeTick;
  tmp.Erosion(k);
  DoStatus('Erosion %dms', [GetTimeTick - tk]);

  tmp.Assign(ori);
  tk := GetTimeTick;
  tmp.Opening(k);
  DoStatus('opening %dms', [GetTimeTick - tk]);

  tmp.Assign(ori);
  tk := GetTimeTick;
  tmp.Closing(k);
  DoStatus('closing %dms', [GetTimeTick - tk]);

  tmp.Assign(ori);
  tk := GetTimeTick;
  tmp.OpeningAndClosing(k);
  DoStatus('OpeningAndClosing %dms', [GetTimeTick - tk]);

  tmp.Assign(ori);
  tk := GetTimeTick;
  tmp.ClosingAndOpening(k);
  DoStatus('ClosingAndOpening %dms', [GetTimeTick - tk]);

  DisposeObject(ori);
  DisposeObject(tmp);
  DisposeObject(k);
end;

procedure InitBinaryzationPreset;
begin
  bin3x3 := TMorphologyBinaryzation.Create;
  bin3x3.SetSize(3, 3, True);
  Bin5x5 := TMorphologyBinaryzation.Create;
  Bin5x5.SetSize(5, 5, True);
  Bin7x7 := TMorphologyBinaryzation.Create;
  Bin7x7.SetSize(7, 7, True);
  Bin9x9 := TMorphologyBinaryzation.Create;
  Bin9x9.SetSize(9, 9, True);
  Bin11x11 := TMorphologyBinaryzation.Create;
  Bin11x11.SetSize(11, 11, True);
  Bin13x13 := TMorphologyBinaryzation.Create;
  Bin13x13.SetSize(13, 13, True);
  Bin15x15 := TMorphologyBinaryzation.Create;
  Bin15x15.SetSize(15, 15, True);
  Bin17x17 := TMorphologyBinaryzation.Create;
  Bin17x17.SetSize(17, 17, True);
  Bin19x19 := TMorphologyBinaryzation.Create;
  Bin19x19.SetSize(19, 19, True);
  Bin21x21 := TMorphologyBinaryzation.Create;
  Bin21x21.SetSize(21, 21, True);
  Bin23x23 := TMorphologyBinaryzation.Create;
  Bin23x23.SetSize(23, 23, True);
  Bin25x25 := TMorphologyBinaryzation.Create;
  Bin25x25.SetSize(25, 25, True);
  Bin51x51 := TMorphologyBinaryzation.Create;
  Bin51x51.SetSize(51, 51, True);
  Bin99x99 := TMorphologyBinaryzation.Create;
  Bin99x99.SetSize(99, 99, True);
end;

procedure FreeBinaryzationPreset;
begin
  DisposeObjectAndNil(bin3x3);
  DisposeObjectAndNil(Bin5x5);
  DisposeObjectAndNil(Bin7x7);
  DisposeObjectAndNil(Bin9x9);
  DisposeObjectAndNil(Bin11x11);
  DisposeObjectAndNil(Bin13x13);
  DisposeObjectAndNil(Bin15x15);
  DisposeObjectAndNil(Bin17x17);
  DisposeObjectAndNil(Bin19x19);
  DisposeObjectAndNil(Bin21x21);
  DisposeObjectAndNil(Bin23x23);
  DisposeObjectAndNil(Bin25x25);
  DisposeObjectAndNil(Bin51x51);
  DisposeObjectAndNil(Bin99x99);
end;
