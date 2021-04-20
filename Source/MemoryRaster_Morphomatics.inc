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

procedure TMorphomaticsList.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DisposeObject(Items[i]);
  Clear;
end;

function TMorphomatics.GetPixel(const X, Y: Integer): TMorphomaticsValue;
begin
  Result := FBits^[ClampInt(X, 0, FWidth - 1) + ClampInt(Y, 0, FHeight - 1) * FWidth];
end;

procedure TMorphomatics.SetPixel(const X, Y: Integer; const Value: TMorphomaticsValue);
begin
  FBits^[ClampInt(X, 0, FWidth - 1) + ClampInt(Y, 0, FHeight - 1) * FWidth] := Value;
end;

function TMorphomatics.GetPixelPtr(const X, Y: Integer): PMorphomaticsValue;
begin
  Result := @FBits^[ClampInt(X, 0, FWidth - 1) + ClampInt(Y, 0, FHeight - 1) * FWidth];
end;

function TMorphomatics.GetScanLine(const Y: Integer): PMorphomaticsBits;
begin
  Result := PMorphomaticsBits(@FBits^[ClampInt(Y, 0, FHeight - 1) * FWidth]);
end;

function TMorphomatics.FindMedian(const N: Integer; arry: TMorphomaticsVector): TMorphomaticsValue;
var
  L, R, K, Y, X: Integer;
  W, X_: TMorphomaticsValue;
begin
  L := 1;
  R := N;
  K := (N div 2) + 1;
  while L < R - 1 do
    begin
      X_ := arry[K];
      Y := L;
      X := R;
      repeat
        while arry[Y] < X_ do
            Y := Y + 1;
        while X_ < arry[X] do
            X := X - 1;
        if Y <= X then
          begin
            W := arry[Y];
            arry[Y] := arry[X];
            arry[X] := W;
            Y := Y + 1;
            X := X - 1;
          end;
      until Y > X;
      if X < K then
          L := Y;
      if K < Y then
          R := X;
    end;
  Result := arry[K];
end;

procedure TMorphomatics.FastSort(var arry: TMorphomaticsVector);
  function Compare_(Left, Right: TMorphomaticsValue): Integer; inline;
  begin
    Result := CompareValue(Left, Right);
  end;

  procedure fastSort_(L, R: Integer);
  var
    i, j: Integer;
    p: TMorphomaticsValue;
  begin
    repeat
      i := L;
      j := R;
      p := arry[(L + R) shr 1];
      repeat
        while Compare_(arry[i], p) < 0 do
            inc(i);
        while Compare_(arry[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
                Swap(arry[i], arry[j]);
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          fastSort_(L, j);
      L := i;
    until i >= R;
  end;

begin
  if length(arry) > 1 then
      fastSort_(0, length(arry) - 1);
end;

constructor TMorphomatics.Create;
begin
  inherited Create;
  LocalParallel := True;
  FBits := nil;
  FWidth := 0;
  FHeight := 0;
end;

destructor TMorphomatics.Destroy;
begin
  FreeBits();
  inherited Destroy;
end;

procedure TMorphomatics.FreeBits;
begin
  if Assigned(FBits) then
    begin
      System.FreeMemory(FBits);
      FBits := nil;
    end;
  FWidth := 0;
  FHeight := 0;
end;

procedure TMorphomatics.SetSize(Width_, Height_: Integer);
begin
  if (Width_ = FWidth) and (Height_ = FHeight) and (FBits <> nil) then
      exit;

  FreeBits();
  FBits := System.GetMemory(Width_ * Height_ * SizeOf(TMorphomaticsValue));
  FWidth := Width_;
  FHeight := Height_;
end;

procedure TMorphomatics.SetSize(Width_, Height_: Integer; Value: TMorphomaticsValue);
begin
  SetSize(Width_, Height_);
  FillValue(Value);
end;

procedure TMorphomatics.SetSizeF(const Width_, Height_: TGeoFloat);
begin
  SetSize(Round(Width_), Round(Height_));
end;

procedure TMorphomatics.SetSizeF(const Width_, Height_: TGeoFloat; const Value: TMorphomaticsValue);
begin
  SetSize(Round(Width_), Round(Height_), Value);
end;

procedure TMorphomatics.SetSizeR(const R: TRectV2);
begin
  SetSize(Round(RectWidth(R)), Round(RectHeight(R)));
end;

procedure TMorphomatics.SetSizeR(const R: TRectV2; const Value: TMorphomaticsValue);
begin
  SetSize(Round(RectWidth(R)), Round(RectHeight(R)), Value);
end;

procedure TMorphomatics.FillValue(Value: TMorphomaticsValue);
var
  i: Integer;
begin
  for i := Width * Height - 1 downto 0 do
      FBits^[i] := Value;
end;

procedure TMorphomatics.FillRandomValue();
var
  i: Integer;
  rnd: TMT19937Random;
begin
  rnd := TMT19937Random.Create;
  for i := Width * Height - 1 downto 0 do
      FBits^[i] := rnd.RandF();
  DisposeObject(rnd);
end;

procedure TMorphomatics.FillValueFromPolygon(Polygon: TVec2List; InsideValue, OutsideValue: TMorphomaticsValue);
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
  FPCParallelFor(TMorphomatics.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMorphomatics.Parallel and LocalParallel, 0, Height - 1, procedure(Y: Integer)
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

function TMorphomatics.Clone: TMorphomatics;
begin
  Result := TMorphomatics.Create;
  Result.LocalParallel := LocalParallel;
  Result.Assign(Self);
end;

procedure TMorphomatics.Assign(sour: TMorphomatics);
begin
  SetSize(sour.Width, sour.Height);
  CopyPtr(@sour.FBits^[0], @FBits^[0], Width * Height * SizeOf(TMorphomaticsValue));
end;

procedure TMorphomatics.SaveToStream(stream: TCoreClassStream);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.WriteInt32(Width);
  m64.WriteInt32(Height);
  m64.WritePtr(@FBits^[0], Width * Height * SizeOf(TMorphomaticsValue));
  m64.Position := 0;
  FastCompressStream(m64, stream);
  DisposeObject(m64);
end;

procedure TMorphomatics.LoadFromStream(stream: TCoreClassStream);
var
  m64: TMemoryStream64;
  W, H: Integer;
begin
  m64 := TMemoryStream64.Create;
  DecompressStream(stream, m64);
  m64.Position := 0;
  W := m64.ReadInt32;
  H := m64.ReadInt32;
  SetSize(W, H);
  m64.ReadPtr(@FBits^[0], Width * Height * SizeOf(TMorphomaticsValue));
  DisposeObject(m64);
end;

procedure TMorphomatics.SwapData(dest: TMorphomatics);
var
  bak_Bits: PMorphomaticsBits;
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

procedure TMorphomatics.Scale(K: TGeoFloat);
var
  tmp: TMemoryRaster;
  morph_: TMorphomatics;
begin
  tmp := BuildViewer(mpGrayscale);
  tmp.Scale(K);
  morph_ := tmp.BuildMorphomatics(mpGrayscale);
  SwapData(morph_);
  DisposeObject(morph_);
  DisposeObject(tmp);
end;

procedure TMorphomatics.FitScale(NewWidth, NewHeight: TGeoFloat);
var
  tmp: TMemoryRaster;
  morph_: TMorphomatics;
begin
  tmp := BuildViewer(mpGrayscale);
  tmp.FitScale(NewWidth, NewHeight);
  morph_ := tmp.BuildMorphomatics(mpGrayscale);
  SwapData(morph_);
  DisposeObject(morph_);
  DisposeObject(tmp);
end;

function TMorphomatics.FitScaleAsNew(NewWidth, NewHeight: TGeoFloat): TMorphomatics;
var
  tmp: TMemoryRaster;
begin
  tmp := BuildViewer(mpGrayscale);
  tmp.FitScale(NewWidth, NewHeight);
  Result := tmp.BuildMorphomatics(mpGrayscale);
  DisposeObject(tmp);
end;

procedure TMorphomatics.DrawTo(MorphPix_: TMorphologyPixel; dest: TMemoryRaster);
begin
  dest.DrawMorphomatics(MorphPix_, Self);
end;

procedure TMorphomatics.DrawTo(dest: TMemoryRaster);
begin
  dest.DrawMorphomatics(mpGrayscale, Self);
end;

function TMorphomatics.BuildViewer(MorphPix_: TMorphologyPixel): TMemoryRaster;
begin
  Result := NewRaster();
  Result.LocalParallel := LocalParallel;
  Result.SetSize(Width, Height, RColor(0, 0, 0));
  DrawTo(MorphPix_, Result);
end;

function TMorphomatics.BuildViewer(): TMemoryRaster;
begin
  Result := BuildViewer(mpGrayscale);
end;

procedure TMorphomatics.BuildViewerFile(MorphPix_: TMorphologyPixel; filename_: SystemString);
begin
  with BuildViewer(MorphPix_) do
    begin
      SaveToFile(filename_);
      Free;
    end;
end;

procedure TMorphomatics.BuildViewerFile(filename_: SystemString);
begin
  with BuildViewer() do
    begin
      SaveToFile(filename_);
      Free;
    end;
end;

procedure TMorphomatics.GetHistogramData(var H: THistogramData);
var
  i, j: Integer;
begin
  Clamp(0.0, 1.0);
  for i := Low(THistogramData) to High(THistogramData) do
      H[i] := 0;

  for j := 0 to Height - 1 do
    for i := 0 to Width - 1 do
        H[Round($FF * Pixel[i, j])] := H[Round($FF * Pixel[i, j])] + 1;
end;

procedure TMorphomatics.BuildHistogramTo(Height_: Integer; hColor: TRColor; output_: TMemoryRaster);
var
  H: THistogramData;
  Max_: Integer;
  i: Integer;
  X1, Y1, X2, Y2: Integer;
begin
  GetHistogramData(H);
  Max_ := H[0];

  for i := 1 to High(THistogramData) do
    if H[i] > Max_ then
        Max_ := H[i];

  output_.SetSize($FF + 1, Height_, RColor(0, 0, 0, $FF));

  for i := Low(THistogramData) to High(THistogramData) do
    begin
      X1 := i;
      Y1 := output_.Height - 1;
      X2 := i;
      Y2 := output_.Height - 1 - Round(H[i] * (Height_ - 1) / Max_);
      if Y2 < Y1 then
          output_.Line(X1, Y1, X2, Y2, hColor, False);
    end;
end;

function TMorphomatics.BuildHistogram(Height_: Integer; hColor: TRColor): TMemoryRaster;
begin
  Result := NewRaster();
  Result.LocalParallel := LocalParallel;
  BuildHistogramTo(Height_, hColor, Result);
end;

procedure TMorphomatics.DrawLine(const X1, Y1, X2, Y2: Integer; const PixelValue_: TMorphomaticsValue; const L: Boolean);
begin
  with TMorphomaticsDraw.Create(FBits, Width, Height, PixelValue_, L) do
    begin
      Line(X1, Y1, X2, Y2);
      Free;
    end;
end;

procedure TMorphomatics.FillBox(const X1, Y1, X2, Y2: Integer; const PixelValue_: TMorphomaticsValue);
begin
  with TMorphomaticsDraw.Create(FBits, Width, Height, PixelValue_, True) do
    begin
      FillBox(X1, Y1, X2, Y2);
      Free;
    end;
end;

function TMorphomatics.BuildHoughLine(const MaxAngle_, AlphaStep_, Treshold_: TGeoFloat; const BestLinesCount_: Integer): THoughLineArry;
begin
  Result := BuildMorphHoughLine(MaxAngle_, AlphaStep_, Treshold_, BestLinesCount_, Self);
end;

procedure TMorphomatics.ProjectionTo(SourMorph_, DestMorph_: TMorphologyPixel; Dst: TMorphomatics; sourRect, DestRect: TV2Rect4; bilinear_sampling: Boolean; alpha: TGeoFloat);
var
  sour_r, dest_r: TMemoryRaster;
begin
  sour_r := BuildViewer(SourMorph_);
  dest_r := Dst.BuildViewer(DestMorph_);
  sour_r.ProjectionTo(dest_r, sourRect, DestRect, bilinear_sampling, alpha);
  dest_r.BuildMorphomaticsTo(DestMorph_, Dst);
  DisposeObject(sour_r);
  DisposeObject(dest_r);
end;

procedure TMorphomatics.ProjectionTo(SourMorph_, DestMorph_: TMorphologyPixel; Dst: TMorphomatics; sourRect, DestRect: TRectV2; bilinear_sampling: Boolean; alpha: TGeoFloat);
begin
  ProjectionTo(SourMorph_, DestMorph_, Dst, TV2Rect4.Init(sourRect, 0), TV2Rect4.Init(DestRect, 0), bilinear_sampling, alpha);
end;

procedure TMorphomatics.Projection(SourMorph_, DestMorph_: TMorphologyPixel; DestRect: TV2Rect4; PixelValue_: TMorphomaticsValue);
var
  sour_r: TMemoryRaster;
  c: TRColor;
begin
  sour_r := BuildViewer(SourMorph_);
  MorphToRColor(DestMorph_, PixelValue_, c);
  sour_r.Projection(DestRect, c);
  sour_r.BuildMorphomaticsTo(DestMorph_, Self);
  DisposeObject(sour_r);
end;

procedure TMorphomatics.ProjectionTo(Dst: TMorphomatics; sourRect, DestRect: TV2Rect4; bilinear_sampling: Boolean; alpha: TGeoFloat);
begin
  ProjectionTo(mpGrayscale, mpGrayscale, Dst, sourRect, DestRect, bilinear_sampling, alpha);
end;

procedure TMorphomatics.ProjectionTo(Dst: TMorphomatics; sourRect, DestRect: TRectV2; bilinear_sampling: Boolean; alpha: TGeoFloat);
begin
  ProjectionTo(mpGrayscale, mpGrayscale, Dst, sourRect, DestRect, bilinear_sampling, alpha);
end;

procedure TMorphomatics.Projection(DestRect: TV2Rect4; PixelValue_: TMorphomaticsValue);
begin
  Projection(mpGrayscale, mpGrayscale, DestRect, PixelValue_);
end;

function TMorphomatics.Width0: Integer;
begin
  if FWidth > 0 then
      Result := FWidth - 1
  else
      Result := 0;
end;

function TMorphomatics.Height0: Integer;
begin
  if FHeight > 0 then
      Result := FHeight - 1
  else
      Result := 0;
end;

function TMorphomatics.SizeOfPoint: TPoint;
begin
  Result := Point(Width, Height);
end;

function TMorphomatics.SizeOf2DPoint: TVec2;
begin
  Result := Vec2(Width, Height);
end;

function TMorphomatics.Size2D: TVec2;
begin
  Result := Vec2(Width, Height);
end;

function TMorphomatics.Size0: TVec2;
begin
  Result := Vec2(Width0, Height0);
end;

function TMorphomatics.BoundsRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

function TMorphomatics.BoundsRect0: TRect;
begin
  Result := Rect(0, 0, Width0, Height0);
end;

function TMorphomatics.BoundsRectV2: TRectV2;
begin
  Result := RectV2(0, 0, Width, Height);
end;

function TMorphomatics.BoundsRectV20: TRectV2;
begin
  Result := RectV2(0, 0, Width0, Height0);
end;

function TMorphomatics.BoundsV2Rect4: TV2Rect4;
begin
  Result := TV2Rect4.Init(BoundsRectV2);
end;

function TMorphomatics.BoundsV2Rect40: TV2Rect4;
begin
  Result := TV2Rect4.Init(BoundsRectV20);
end;

function TMorphomatics.Centroid: TVec2;
begin
  Result := RectCentre(BoundsRectV2);
end;

function TMorphomatics.Centre: TVec2;
begin
  Result := RectCentre(BoundsRectV2);
end;

function TMorphomatics.InHere(const X, Y: Integer): Boolean;
begin
  Result := PointInRect(X, Y, 0, 0, Width0, Height0);
end;

procedure TMorphomatics.SigmaGaussian(const SIGMA: TGeoFloat; const SigmaGaussianKernelFactor: Integer);
var
  tmp: TMorphomatics;
begin
  tmp := TMorphomatics.Create;
  tmp.SetSize(Width, Height);

  MorphSigmaGaussianSampler(TMorphomatics.Parallel and LocalParallel, Self, tmp, SIGMA, SigmaGaussianKernelFactor);

  SwapData(tmp);
  DisposeObject(tmp);
end;

procedure TMorphomatics.Average(BoxW, BoxH: Integer);
var
  tmp: TMorphomatics;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(Y: Integer);
  var
    X, fY, fX: Integer;
    sum_: TMorphomaticsValue;
  begin
    for X := 0 to Width - 1 do
      begin
        sum_ := 0;
        for fY := -BoxH to BoxH do
          for fX := -BoxW to BoxW do
              sum_ := sum_ + Pixel[X + fX, Y + fY];
        sum_ := sum_ / ((2 * BoxH + 1) * (2 * BoxW + 1));
        tmp[X, Y] := sum_;
      end;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    Y: Integer;
    X, fY, fX, K: Integer;
    sum_: TMorphomaticsValue;
  begin
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        begin
          sum_ := 0;
          for fY := -BoxH to BoxH do
            for fX := -BoxW to BoxW do
                sum_ := sum_ + Pixel[X + fX, Y + fY];
          sum_ := sum_ / ((2 * BoxH + 1) * (2 * BoxW + 1));
          tmp[X, Y] := sum_;
        end;
  end;
{$ENDIF Parallel}


begin
  tmp := TMorphomatics.Create;
  tmp.SetSize(Width, Height);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMorphomatics.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMorphomatics.Parallel and LocalParallel, 0, Height - 1, procedure(Y: Integer)
    var
      X, fY, fX: Integer;
      sum_: TMorphomaticsValue;
    begin
      for X := 0 to Width - 1 do
        begin
          sum_ := 0;
          for fY := -BoxH to BoxH do
            for fX := -BoxW to BoxW do
                sum_ := sum_ + Pixel[X + fX, Y + fY];
          sum_ := sum_ / ((2 * BoxH + 1) * (2 * BoxW + 1));
          tmp[X, Y] := sum_;
        end;
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  SwapData(tmp);
  DisposeObject(tmp);
end;

procedure TMorphomatics.WeightedAVG(BoxW, BoxH: Integer);
var
  tmp: TMorphomatics;
  Mask: TMorphomaticsMatrix;
  maxDistance, weigth: TMorphomaticsValue;

  procedure BuildMask;
  var
    Y, X: Integer;
  begin
    SetLength(Mask, 2 * BoxH + 1, 2 * BoxW + 1);
    for Y := -BoxH to BoxH do
      for X := -BoxW to BoxW do
          Mask[Y + BoxH, X + BoxW] := Sqrt(Sqr(Y) + Sqr(X));
    maxDistance := Mask[0, 0];
    weigth := 0;
    for Y := 0 to 2 * BoxH do
      for X := 0 to 2 * BoxW do
        begin
          Mask[Y, X] := (maxDistance - Mask[Y, X]) / maxDistance;
          weigth := weigth + Mask[Y, X];
        end;
  end;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(Y: Integer);
  var
    X, fY, fX: Integer;
    sum_: TMorphomaticsValue;
  begin
    for X := 0 to Width - 1 do
      begin
        sum_ := 0;
        for fY := -BoxH to BoxH do
          for fX := -BoxW to BoxW do
              sum_ := sum_ + Mask[fY + BoxH, fX + BoxW] * Pixel[X + fX, Y + fY];
        tmp[X, Y] := sum_ / weigth;
      end;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    Y: Integer;
    X, fY, fX, K: Integer;
    sum_: TMorphomaticsValue;
  begin
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        begin
          sum_ := 0;
          for fY := -BoxH to BoxH do
            for fX := -BoxW to BoxW do
                sum_ := sum_ + Mask[fY + BoxH, fX + BoxW] * Pixel[X + fX, Y + fY];
          tmp[X, Y] := sum_ / weigth;
        end;
  end;
{$ENDIF Parallel}


begin
  tmp := TMorphomatics.Create;
  tmp.SetSize(Width, Height);

  BuildMask();

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMorphomatics.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMorphomatics.Parallel and LocalParallel, 0, Height - 1, procedure(Y: Integer)
    var
      X, fY, fX: Integer;
      sum_: TMorphomaticsValue;
    begin
      for X := 0 to Width - 1 do
        begin
          sum_ := 0;
          for fY := -BoxH to BoxH do
            for fX := -BoxW to BoxW do
                sum_ := sum_ + Mask[fY + BoxH, fX + BoxW] * Pixel[X + fX, Y + fY];
          tmp[X, Y] := sum_ / weigth;
        end;
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  SetLength(Mask, 0, 0);
  SwapData(tmp);
  DisposeObject(tmp);
end;

procedure TMorphomatics.GeometricMean(BoxW, BoxH: Integer);
var
  tmp: TMorphomatics;
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(Y: Integer);
  var
    X, fY, fX: Integer;
    p: Extended;
  begin
    for X := 0 to Width - 1 do
      begin
        p := 1;
        for fY := -BoxH to BoxH do
          for fX := -BoxW to BoxW do
              p := p * Pixel[X + fX, Y + fY];
        if p > 0 then
            p := Power(p, 1 / ((2 * BoxH + 1) * (2 * BoxW + 1)))
        else
            p := 0;
        tmp[X, Y] := p;
      end;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    Y: Integer;
    X, fY, fX, K: Integer;
    p: Extended;
  begin
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        begin
          p := 1;
          for fY := -BoxH to BoxH do
            for fX := -BoxW to BoxW do
                p := p * Pixel[X + fX, Y + fY];
          if p > 0 then
              p := Power(p, 1 / ((2 * BoxH + 1) * (2 * BoxW + 1)))
          else
              p := 0;
          tmp[X, Y] := p;
        end;
  end;
{$ENDIF Parallel}


begin
  tmp := TMorphomatics.Create;
  tmp.SetSize(Width, Height);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMorphomatics.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMorphomatics.Parallel and LocalParallel, 0, Height - 1, procedure(Y: Integer)
    var
      X, fY, fX: Integer;
      p: Extended;
    begin
      for X := 0 to Width - 1 do
        begin
          p := 1;
          for fY := -BoxH to BoxH do
            for fX := -BoxW to BoxW do
                p := p * Pixel[X + fX, Y + fY];
          if p > 0 then
              p := Power(p, 1 / ((2 * BoxH + 1) * (2 * BoxW + 1)))
          else
              p := 0;
          tmp[X, Y] := p;
        end;
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  SwapData(tmp);
  DisposeObject(tmp);
end;

procedure TMorphomatics.Median(BoxW, BoxH: Integer);
var
  tmp: TMorphomatics;
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(Y: Integer);
  var
    X, fY, fX, K: Integer;
    tmpVec: TMorphomaticsVector;
  begin
    SetLength(tmpVec, (2 * BoxH + 1) * (2 * BoxW + 1) + 1);
    for X := 0 to Width - 1 do
      begin
        K := 0;
        for fY := -BoxH to BoxH do
          for fX := -BoxW to BoxW do
            begin
              inc(K);
              tmpVec[K] := Pixel[X + fX, Y + fY];
            end;
        tmp[X, Y] := FindMedian((2 * BoxH + 1) * (2 * BoxW + 1), tmpVec);
      end;
    SetLength(tmpVec, 0);
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    Y: Integer;
    X, fY, fX, K: Integer;
    tmpVec: TMorphomaticsVector;
  begin
    SetLength(tmpVec, (2 * BoxH + 1) * (2 * BoxW + 1) + 1);
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        begin
          K := 0;
          for fY := -BoxH to BoxH do
            for fX := -BoxW to BoxW do
              begin
                inc(K);
                tmpVec[K] := Pixel[X + fX, Y + fY];
              end;
          tmp[X, Y] := FindMedian((2 * BoxH + 1) * (2 * BoxW + 1), tmpVec);
        end;
    SetLength(tmpVec, 0);
  end;
{$ENDIF Parallel}


begin
  tmp := TMorphomatics.Create;
  tmp.SetSize(Width, Height);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMorphomatics.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMorphomatics.Parallel and LocalParallel, 0, Height - 1, procedure(Y: Integer)
    var
      X, fY, fX, K: Integer;
      tmpVec: TMorphomaticsVector;
    begin
      SetLength(tmpVec, (2 * BoxH + 1) * (2 * BoxW + 1) + 1);
      for X := 0 to Width - 1 do
        begin
          K := 0;
          for fY := -BoxH to BoxH do
            for fX := -BoxW to BoxW do
              begin
                inc(K);
                tmpVec[K] := Pixel[X + fX, Y + fY];
              end;
          tmp[X, Y] := FindMedian((2 * BoxH + 1) * (2 * BoxW + 1), tmpVec);
        end;
      SetLength(tmpVec, 0);
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  SwapData(tmp);
  DisposeObject(tmp);
end;

procedure TMorphomatics.Maximum(BoxW, BoxH: Integer);
var
  tmp: TMorphomatics;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(Y: Integer);
  var
    X, fY, fX, K: Integer;
    Max_: TMorphomaticsValue;
    tmpVec: TMorphomaticsVector;
  begin
    SetLength(tmpVec, (2 * BoxH + 1) * (2 * BoxW + 1) + 1);
    for X := 0 to Width - 1 do
      begin
        K := 0;
        for fY := -BoxH to BoxH do
          for fX := -BoxW to BoxW do
            begin
              inc(K);
              tmpVec[K] := Pixel[X + fX, Y + fY];
            end;
        Max_ := tmpVec[1];
        for K := 1 to (2 * BoxH + 1) * (2 * BoxW + 1) do
          if tmpVec[K] > Max_ then
              Max_ := tmpVec[K];
        tmp[X, Y] := Max_;
      end;
    SetLength(tmpVec, 0);
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    Y: Integer;
    X, fY, fX, K: Integer;
    Max_: TMorphomaticsValue;
    tmpVec: TMorphomaticsVector;
  begin
    SetLength(tmpVec, (2 * BoxH + 1) * (2 * BoxW + 1) + 1);
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        begin
          K := 0;
          for fY := -BoxH to BoxH do
            for fX := -BoxW to BoxW do
              begin
                inc(K);
                tmpVec[K] := Pixel[X + fX, Y + fY];
              end;
          Max_ := tmpVec[1];
          for K := 1 to (2 * BoxH + 1) * (2 * BoxW + 1) do
            if tmpVec[K] > Max_ then
                Max_ := tmpVec[K];
          tmp[X, Y] := Max_;
        end;
    SetLength(tmpVec, 0);
  end;
{$ENDIF Parallel}


begin
  tmp := TMorphomatics.Create;
  tmp.SetSize(Width, Height);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMorphomatics.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMorphomatics.Parallel and LocalParallel, 0, Height - 1, procedure(Y: Integer)
    var
      X, fY, fX, K: Integer;
      Max_: TMorphomaticsValue;
      tmpVec: TMorphomaticsVector;
    begin
      SetLength(tmpVec, (2 * BoxH + 1) * (2 * BoxW + 1) + 1);
      for X := 0 to Width - 1 do
        begin
          K := 0;
          for fY := -BoxH to BoxH do
            for fX := -BoxW to BoxW do
              begin
                inc(K);
                tmpVec[K] := Pixel[X + fX, Y + fY];
              end;
          Max_ := tmpVec[1];
          for K := 1 to (2 * BoxH + 1) * (2 * BoxW + 1) do
            if tmpVec[K] > Max_ then
                Max_ := tmpVec[K];
          tmp[X, Y] := Max_;
        end;
      SetLength(tmpVec, 0);
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  SwapData(tmp);
  DisposeObject(tmp);
end;

procedure TMorphomatics.Minimum(BoxW, BoxH: Integer);
var
  tmp: TMorphomatics;
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(Y: Integer);
  var
    X, fY, fX, K: Integer;
    Min_: TMorphomaticsValue;
    tmpVec: TMorphomaticsVector;
  begin
    SetLength(tmpVec, (2 * BoxH + 1) * (2 * BoxW + 1) + 1);

    for X := 0 to Width - 1 do
      begin
        K := 0;
        for fY := -BoxH to BoxH do
          for fX := -BoxW to BoxW do
            begin
              inc(K);
              tmpVec[K] := Pixel[X + fX, Y + fY];
            end;
        Min_ := tmpVec[1];
        for K := 1 to (2 * BoxH + 1) * (2 * BoxW + 1) do
          if tmpVec[K] < Min_ then
              Min_ := tmpVec[K];
        tmp[X, Y] := Min_;
      end;

    SetLength(tmpVec, 0);
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    Y: Integer;
    X, fY, fX, K: Integer;
    Min_: TMorphomaticsValue;
    tmpVec: TMorphomaticsVector;
  begin
    SetLength(tmpVec, (2 * BoxH + 1) * (2 * BoxW + 1) + 1);

    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        begin
          K := 0;
          for fY := -BoxH to BoxH do
            for fX := -BoxW to BoxW do
              begin
                inc(K);
                tmpVec[K] := Pixel[X + fX, Y + fY];
              end;
          Min_ := tmpVec[1];
          for K := 1 to (2 * BoxH + 1) * (2 * BoxW + 1) do
            if tmpVec[K] < Min_ then
                Min_ := tmpVec[K];
          tmp[X, Y] := Min_;
        end;

    SetLength(tmpVec, 0);
  end;
{$ENDIF Parallel}


begin
  tmp := TMorphomatics.Create;
  tmp.SetSize(Width, Height);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMorphomatics.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMorphomatics.Parallel and LocalParallel, 0, Height - 1, procedure(Y: Integer)
    var
      X, fY, fX, K: Integer;
      Min_: TMorphomaticsValue;
      tmpVec: TMorphomaticsVector;
    begin
      SetLength(tmpVec, (2 * BoxH + 1) * (2 * BoxW + 1) + 1);

      for X := 0 to Width - 1 do
        begin
          K := 0;
          for fY := -BoxH to BoxH do
            for fX := -BoxW to BoxW do
              begin
                inc(K);
                tmpVec[K] := Pixel[X + fX, Y + fY];
              end;
          Min_ := tmpVec[1];
          for K := 1 to (2 * BoxH + 1) * (2 * BoxW + 1) do
            if tmpVec[K] < Min_ then
                Min_ := tmpVec[K];
          tmp[X, Y] := Min_;
        end;

      SetLength(tmpVec, 0);
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  SwapData(tmp);
  DisposeObject(tmp);
end;

procedure TMorphomatics.MiddlePoint(BoxW, BoxH: Integer);
var
  tmp: TMorphomatics;
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(Y: Integer);
  var
    X, fY, fX, K: Integer;
    Min_, Max_: TMorphomaticsValue;
    tmpVec: TMorphomaticsVector;
  begin
    SetLength(tmpVec, (2 * BoxH + 1) * (2 * BoxW + 1) + 1);

    for X := 0 to Width - 1 do
      begin
        K := 0;
        for fY := -BoxH to BoxH do
          for fX := -BoxW to BoxW do
            begin
              inc(K);
              tmpVec[K] := Pixel[X + fX, Y + fY];
            end;
        Min_ := tmpVec[1];
        Max_ := tmpVec[1];
        for K := 1 to (2 * BoxH + 1) * (2 * BoxW + 1) do
          begin
            if tmpVec[K] < Min_ then
                Min_ := tmpVec[K];
            if tmpVec[K] > Max_ then
                Max_ := tmpVec[K];
          end;
        tmp[X, Y] := (Max_ + Min_) * 0.5;
      end;

    SetLength(tmpVec, 0);
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    Y: Integer;
    X, fY, fX, K: Integer;
    Min_, Max_: TMorphomaticsValue;
    tmpVec: TMorphomaticsVector;
  begin
    SetLength(tmpVec, (2 * BoxH + 1) * (2 * BoxW + 1) + 1);

    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        begin
          K := 0;
          for fY := -BoxH to BoxH do
            for fX := -BoxW to BoxW do
              begin
                inc(K);
                tmpVec[K] := Pixel[X + fX, Y + fY];
              end;
          Min_ := tmpVec[1];
          Max_ := tmpVec[1];
          for K := 1 to (2 * BoxH + 1) * (2 * BoxW + 1) do
            begin
              if tmpVec[K] < Min_ then
                  Min_ := tmpVec[K];
              if tmpVec[K] > Max_ then
                  Max_ := tmpVec[K];
            end;
          tmp[X, Y] := (Max_ + Min_) * 0.5;
        end;

    SetLength(tmpVec, 0);
  end;
{$ENDIF Parallel}


begin
  tmp := TMorphomatics.Create;
  tmp.SetSize(Width, Height);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMorphomatics.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMorphomatics.Parallel and LocalParallel, 0, Height - 1, procedure(Y: Integer)
    var
      X, fY, fX, K: Integer;
      Min_, Max_: TMorphomaticsValue;
      tmpVec: TMorphomaticsVector;
    begin
      SetLength(tmpVec, (2 * BoxH + 1) * (2 * BoxW + 1) + 1);

      for X := 0 to Width - 1 do
        begin
          K := 0;
          for fY := -BoxH to BoxH do
            for fX := -BoxW to BoxW do
              begin
                inc(K);
                tmpVec[K] := Pixel[X + fX, Y + fY];
              end;
          Min_ := tmpVec[1];
          Max_ := tmpVec[1];
          for K := 1 to (2 * BoxH + 1) * (2 * BoxW + 1) do
            begin
              if tmpVec[K] < Min_ then
                  Min_ := tmpVec[K];
              if tmpVec[K] > Max_ then
                  Max_ := tmpVec[K];
            end;
          tmp[X, Y] := (Max_ + Min_) * 0.5;
        end;

      SetLength(tmpVec, 0);
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  SwapData(tmp);
  DisposeObject(tmp);
end;

procedure TMorphomatics.TruncatedAVG(BoxW, BoxH, d: Integer);
var
  tmp: TMorphomatics;
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(Y: Integer);
  var
    X, fY, fX, K: Integer;
    tmpVec: TMorphomaticsVector;
    sum_: TMorphomaticsValue;
  begin
    SetLength(tmpVec, (2 * BoxH + 1) * (2 * BoxW + 1) + 1);

    for X := 0 to Width - 1 do
      begin
        K := 0;
        tmpVec[K] := 0;
        for fY := -BoxH to BoxH do
          for fX := -BoxW to BoxW do
            begin
              inc(K);
              tmpVec[K] := Pixel[X + fX, Y + fY];
            end;

        // sort
        FastSort(tmpVec);

        // Truncated sum
        sum_ := 0;
        for K := d + 1 to (2 * BoxH + 1) * (2 * BoxW + 1) - d do
            sum_ := sum_ + tmpVec[K];

        tmp[X, Y] := sum_ / ((2 * BoxH + 1) * (2 * BoxW + 1) - 2 * d);
      end;

    SetLength(tmpVec, 0);
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    Y: Integer;
    X, fY, fX, K: Integer;
    tmpVec: TMorphomaticsVector;
    sum_: TMorphomaticsValue;
  begin
    SetLength(tmpVec, (2 * BoxH + 1) * (2 * BoxW + 1) + 1);

    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        begin
          K := 0;
          tmpVec[K] := 0;
          for fY := -BoxH to BoxH do
            for fX := -BoxW to BoxW do
              begin
                inc(K);
                tmpVec[K] := Pixel[X + fX, Y + fY];
              end;

          // sort
          FastSort(tmpVec);

          // Truncated sum
          sum_ := 0;
          for K := d + 1 to (2 * BoxH + 1) * (2 * BoxW + 1) - d do
              sum_ := sum_ + tmpVec[K];

          tmp[X, Y] := sum_ / ((2 * BoxH + 1) * (2 * BoxW + 1) - 2 * d);
        end;

    SetLength(tmpVec, 0);
  end;
{$ENDIF Parallel}


begin
  tmp := TMorphomatics.Create;
  tmp.SetSize(Width, Height);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMorphomatics.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMorphomatics.Parallel and LocalParallel, 0, Height - 1, procedure(Y: Integer)
    var
      X, fY, fX, K: Integer;
      tmpVec: TMorphomaticsVector;
      sum_: TMorphomaticsValue;
    begin
      SetLength(tmpVec, (2 * BoxH + 1) * (2 * BoxW + 1) + 1);

      for X := 0 to Width - 1 do
        begin
          K := 0;
          tmpVec[K] := 0;
          for fY := -BoxH to BoxH do
            for fX := -BoxW to BoxW do
              begin
                inc(K);
                tmpVec[K] := Pixel[X + fX, Y + fY];
              end;

          // sort
          FastSort(tmpVec);

          // Truncated sum
          sum_ := 0;
          for K := d + 1 to (2 * BoxH + 1) * (2 * BoxW + 1) - d do
              sum_ := sum_ + tmpVec[K];

          tmp[X, Y] := sum_ / ((2 * BoxH + 1) * (2 * BoxW + 1) - 2 * d);
        end;

      SetLength(tmpVec, 0);
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  SwapData(tmp);
  DisposeObject(tmp);
end;

procedure TMorphomatics.Previtt(AdditiveToOriginal: Boolean);
const
  PrevittMaskX: array [1 .. 3, 1 .. 3] of ShortInt = ((-1, 0, 1), (-1, 0, 1), (-1, 0, 1));
  PrevittMaskY: array [1 .. 3, 1 .. 3] of ShortInt = ((1, 1, 1), (0, 0, 0), (-1, -1, -1));
var
  tmp: TMorphomatics;
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(Y: Integer);
  var
    X, fY, fX: Integer;
    R: TMorphomaticsValue;
  begin
    for X := 0 to Width - 1 do
      begin
        R := 0;
        for fY := -1 to 1 do
          for fX := -1 to 1 do
              R := R + PrevittMaskX[fY + 2, fX + 2] * Pixel[X + fX, Y + fY] + PrevittMaskY[fY + 2, fX + 2] * Pixel[X + fX, Y + fY];
        if AdditiveToOriginal then
            R := Pixel[X, Y] + R;
        tmp[X, Y] := R;
      end;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    Y: Integer;
    X, fY, fX: Integer;
    R: TMorphomaticsValue;
  begin
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        begin
          R := 0;
          for fY := -1 to 1 do
            for fX := -1 to 1 do
                R := R + PrevittMaskX[fY + 2, fX + 2] * Pixel[X + fX, Y + fY] + PrevittMaskY[fY + 2, fX + 2] * Pixel[X + fX, Y + fY];
          if AdditiveToOriginal then
              R := Pixel[X, Y] + R;
          tmp[X, Y] := R;
        end;
  end;
{$ENDIF Parallel}


begin
  tmp := TMorphomatics.Create;
  tmp.SetSize(Width, Height);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMorphomatics.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMorphomatics.Parallel and LocalParallel, 0, Height - 1, procedure(Y: Integer)
    var
      X, fY, fX: Integer;
      R: TMorphomaticsValue;
    begin
      for X := 0 to Width - 1 do
        begin
          R := 0;
          for fY := -1 to 1 do
            for fX := -1 to 1 do
                R := R + PrevittMaskX[fY + 2, fX + 2] * Pixel[X + fX, Y + fY] + PrevittMaskY[fY + 2, fX + 2] * Pixel[X + fX, Y + fY];
          if AdditiveToOriginal then
              R := Pixel[X, Y] + R;
          tmp[X, Y] := R;
        end;
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  SwapData(tmp);
  DisposeObject(tmp);
end;

procedure TMorphomatics.Sobel(AdditiveToOriginal: Boolean);
const
  SobelMaskX: array [1 .. 3, 1 .. 3] of ShortInt = ((-1, 0, 1), (-2, 0, 2), (-1, 0, 1));
  SobelMaskY: array [1 .. 3, 1 .. 3] of ShortInt = ((1, 2, 1), (0, 0, 0), (-1, -2, -1));
var
  tmp: TMorphomatics;
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(Y: Integer);
  var
    X, fY, fX: Integer;
    R: TMorphomaticsValue;
  begin
    for X := 0 to Width - 1 do
      begin
        R := 0;
        for fY := -1 to 1 do
          for fX := -1 to 1 do
              R := R + SobelMaskX[fY + 2, fX + 2] * Pixel[X + fX, Y + fY] + SobelMaskY[fY + 2, fX + 2] * Pixel[X + fX, Y + fY];
        if AdditiveToOriginal then
            R := Pixel[X, Y] + R;
        tmp[X, Y] := R;
      end;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    Y: Integer;
    X, fY, fX: Integer;
    R: TMorphomaticsValue;
  begin
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        begin
          R := 0;
          for fY := -1 to 1 do
            for fX := -1 to 1 do
                R := R + SobelMaskX[fY + 2, fX + 2] * Pixel[X + fX, Y + fY] + SobelMaskY[fY + 2, fX + 2] * Pixel[X + fX, Y + fY];
          if AdditiveToOriginal then
              R := Pixel[X, Y] + R;
          tmp[X, Y] := R;
        end;
  end;
{$ENDIF Parallel}


begin
  tmp := TMorphomatics.Create;
  tmp.SetSize(Width, Height);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMorphomatics.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMorphomatics.Parallel and LocalParallel, 0, Height - 1, procedure(Y: Integer)
    var
      X, fY, fX: Integer;
      R: TMorphomaticsValue;
    begin
      for X := 0 to Width - 1 do
        begin
          R := 0;
          for fY := -1 to 1 do
            for fX := -1 to 1 do
                R := R + SobelMaskX[fY + 2, fX + 2] * Pixel[X + fX, Y + fY] + SobelMaskY[fY + 2, fX + 2] * Pixel[X + fX, Y + fY];
          if AdditiveToOriginal then
              R := Pixel[X, Y] + R;
          tmp[X, Y] := R;
        end;
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  SwapData(tmp);
  DisposeObject(tmp);
end;

procedure TMorphomatics.Sharr(AdditiveToOriginal: Boolean);
const
  SharrMaskX: array [1 .. 3, 1 .. 3] of ShortInt = ((-3, 0, 3), (-10, 0, 10), (-3, 0, 3));
  SharrMaskY: array [1 .. 3, 1 .. 3] of ShortInt = ((3, 10, 3), (0, 0, 0), (-3, -10, -3));
var
  tmp: TMorphomatics;
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(Y: Integer);
  var
    X, fY, fX: Integer;
    R: TMorphomaticsValue;
  begin
    for X := 0 to Width - 1 do
      begin
        R := 0;
        for fY := -1 to 1 do
          for fX := -1 to 1 do
              R := R + SharrMaskX[fY + 2, fX + 2] * Pixel[X + fX, Y + fY] + SharrMaskY[fY + 2, fX + 2] * Pixel[X + fX, Y + fY];
        if AdditiveToOriginal then
            R := Pixel[X, Y] + R;
        tmp[X, Y] := R;
      end;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    Y: Integer;
    X, fY, fX: Integer;
    R: TMorphomaticsValue;
  begin
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        begin
          R := 0;
          for fY := -1 to 1 do
            for fX := -1 to 1 do
                R := R + SharrMaskX[fY + 2, fX + 2] * Pixel[X + fX, Y + fY] + SharrMaskY[fY + 2, fX + 2] * Pixel[X + fX, Y + fY];
          if AdditiveToOriginal then
              R := Pixel[X, Y] + R;
          tmp[X, Y] := R;
        end;
  end;
{$ENDIF Parallel}


begin
  tmp := TMorphomatics.Create;
  tmp.SetSize(Width, Height);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMorphomatics.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMorphomatics.Parallel and LocalParallel, 0, Height - 1, procedure(Y: Integer)
    var
      X, fY, fX: Integer;
      R: TMorphomaticsValue;
    begin
      for X := 0 to Width - 1 do
        begin
          R := 0;
          for fY := -1 to 1 do
            for fX := -1 to 1 do
                R := R + SharrMaskX[fY + 2, fX + 2] * Pixel[X + fX, Y + fY] + SharrMaskY[fY + 2, fX + 2] * Pixel[X + fX, Y + fY];
          if AdditiveToOriginal then
              R := Pixel[X, Y] + R;
          tmp[X, Y] := R;
        end;
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  SwapData(tmp);
  DisposeObject(tmp);
end;

procedure TMorphomatics.Laplace(AdditiveToOriginal: Boolean);
const
  LaplaceMask: array [1 .. 3, 1 .. 3] of ShortInt = ((1, 1, 1), (1, -8, 1), (1, 1, 1));
var
  tmp: TMorphomatics;
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(Y: Integer);
  var
    X, fY, fX: Integer;
    R: TMorphomaticsValue;
  begin
    for X := 0 to Width - 1 do
      begin
        R := 0;
        for fY := -1 to 1 do
          for fX := -1 to 1 do
              R := R + LaplaceMask[fY + 2, fX + 2] * Pixel[X + fX, Y + fY];
        if AdditiveToOriginal then
            R := Pixel[X, Y] + R;
        tmp[X, Y] := R;
      end;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    Y: Integer;
    X, fY, fX: Integer;
    R: TMorphomaticsValue;
  begin
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        begin
          R := 0;
          for fY := -1 to 1 do
            for fX := -1 to 1 do
                R := R + LaplaceMask[fY + 2, fX + 2] * Pixel[X + fX, Y + fY];
          if AdditiveToOriginal then
              R := Pixel[X, Y] + R;
          tmp[X, Y] := R;
        end;
  end;
{$ENDIF Parallel}


begin
  tmp := TMorphomatics.Create;
  tmp.SetSize(Width, Height);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMorphomatics.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMorphomatics.Parallel and LocalParallel, 0, Height - 1, procedure(Y: Integer)
    var
      X, fY, fX: Integer;
      R: TMorphomaticsValue;
    begin
      for X := 0 to Width - 1 do
        begin
          R := 0;
          for fY := -1 to 1 do
            for fX := -1 to 1 do
                R := R + LaplaceMask[fY + 2, fX + 2] * Pixel[X + fX, Y + fY];
          if AdditiveToOriginal then
              R := Pixel[X, Y] + R;
          tmp[X, Y] := R;
        end;
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  SwapData(tmp);
  DisposeObject(tmp);
end;

procedure TMorphomatics.ProcessFilter(filter: TMorphFilter);
begin
  case filter of
    mfAverage: Average(5, 5);
    mfWeightedAVG: WeightedAVG(5, 5);
    mfGeometricMean: GeometricMean(5, 5);
    mfMedian: Median(5, 5);
    mfMax: Maximum(5, 5);
    mfMin: Minimum(5, 5);
    mfMiddlePoint: MiddlePoint(5, 5);
    mfTruncatedAVG: TruncatedAVG(5, 5, 2);
    mfPrevitt: Previtt(False);
    mfSobel: Sobel(False);
    mfSharr: Sharr(False);
    mfLaplace: Laplace(False);
  end;
end;

procedure TMorphomatics.Linear(K, B: TMorphomaticsValue);
var
  i: Integer;
begin
  for i := Width * Height - 1 downto 0 do
      FBits^[i] := K * FBits^[i] + B;
end;

procedure TMorphomatics.Logarithms(c: TMorphomaticsValue);
var
  i: Integer;
begin
  for i := Width * Height - 1 downto 0 do
      FBits^[i] := c * log2(FBits^[i] + 1);
end;

procedure TMorphomatics.Gamma(c, Gamma: TMorphomaticsValue);
var
  i: Integer;
begin
  for i := Width * Height - 1 downto 0 do
    if FBits^[i] > 0 then
        FBits^[i] := c * Power(FBits^[i], Gamma)
    else
        FBits^[i] := 0;
end;

procedure TMorphomatics.HistogramEqualization;
const
  C_HistogramSize = $FF;
var
  i: Integer;
  buff: array [0 .. C_HistogramSize] of TMorphomaticsValue;
begin
  Clamp(0.0, 1.0);
  for i := 0 to C_HistogramSize do
      buff[i] := 0;
  for i := Width * Height - 1 downto 0 do
      buff[Round(C_HistogramSize * FBits^[i])] := buff[Round(C_HistogramSize * FBits^[i])] + 1;
  for i := 0 to C_HistogramSize do
      buff[i] := buff[i] / (Width * Height - 1);
  for i := 1 to C_HistogramSize do
      buff[i] := buff[i - 1] + buff[i];
  for i := Width * Height - 1 downto 0 do
      FBits^[i] := buff[Round(C_HistogramSize * FBits^[i])];
end;

procedure TMorphomatics.Contrast(K: TMorphomaticsValue);
const
  C_HistogramSize = $FF;
var
  avg_, delta, temp: TMorphomaticsValue;
  i, j: Integer;
  buff: array [0 .. C_HistogramSize] of TMorphomaticsValue;
begin
  Clamp(0.0, 1.0);
  avg_ := 0;
  for i := Width * Height - 1 downto 0 do
      avg_ := avg_ + FBits^[i];
  avg_ := Round(avg_ / (Width * Height - 1) * C_HistogramSize);
  for i := 0 to C_HistogramSize do
    begin
      delta := i - avg_;
      temp := umlClamp(avg_ + K * delta, 0, C_HistogramSize);
      buff[i] := temp / C_HistogramSize;
    end;
  for i := Width * Height - 1 downto 0 do
      FBits^[i] := buff[Round(FBits^[i] * C_HistogramSize)];
end;

procedure TMorphomatics.Gradient(level: Byte);
var
  i: Integer;
  f: TMorphomaticsValue;
begin
  f := $FF / level;
  for i := Width * Height - 1 downto 0 do
      FBits^[i] := Trunc(Trunc(Trunc(umlClamp(FBits^[i], 0.0, 1.0) * $FF) / f) * f) / $FF;
end;

procedure TMorphomatics.Clamp(MinV, MaxV: TMorphomaticsValue);
var
  i: Integer;
begin
  for i := Width * Height - 1 downto 0 do
      FBits^[i] := umlClamp(FBits^[i], MinV, MaxV);
end;

procedure TMorphomatics.Invert;
var
  Max_: TMorphomaticsValue;
  i: Integer;
begin
  Max_ := FBits^[0];
  for i := Width * Height - 1 downto 1 do
      Max_ := umlMax(FBits^[i], Max_);
  for i := Width * Height - 1 downto 0 do
      FBits^[i] := Max_ - FBits^[i];
end;

procedure TMorphomatics.ADD_(Morph: TMorphomatics);
var
  i, j: Integer;
begin
  for j := 0 to umlMin(Height, Morph.Height) - 1 do
    for i := 0 to umlMin(Width, Morph.Width) - 1 do
        Pixel[i, j] := Pixel[i, j] + Morph[i, j];
end;

procedure TMorphomatics.SUB_(Morph: TMorphomatics);
var
  i, j: Integer;
begin
  for j := 0 to umlMin(Height, Morph.Height) - 1 do
    for i := 0 to umlMin(Width, Morph.Width) - 1 do
        Pixel[i, j] := Pixel[i, j] - Morph[i, j];
end;

procedure TMorphomatics.MUL_(Morph: TMorphomatics);
var
  i, j: Integer;
begin
  for j := 0 to umlMin(Height, Morph.Height) - 1 do
    for i := 0 to umlMin(Width, Morph.Width) - 1 do
        Pixel[i, j] := Pixel[i, j] * Morph[i, j];
end;

procedure TMorphomatics.DIV_(Morph: TMorphomatics);
var
  i, j: Integer;
begin
  for j := 0 to umlMin(Height, Morph.Height) - 1 do
    for i := 0 to umlMin(Width, Morph.Width) - 1 do
        Pixel[i, j] := Pixel[i, j] / Morph[i, j];
end;

procedure TMorphomatics.ADD_(f: TMorphomaticsValue);
var
  i: Integer;
begin
  for i := Width * Height - 1 downto 0 do
      FBits^[i] := FBits^[i] + f;
end;

procedure TMorphomatics.SUB_(f: TMorphomaticsValue);
var
  i: Integer;
begin
  for i := Width * Height - 1 downto 0 do
      FBits^[i] := FBits^[i] - f;
end;

procedure TMorphomatics.MUL_(f: TMorphomaticsValue);
var
  i: Integer;
begin
  for i := Width * Height - 1 downto 0 do
      FBits^[i] := FBits^[i] * f;
end;

procedure TMorphomatics.DIV_(f: TMorphomaticsValue);
var
  i: Integer;
begin
  for i := Width * Height - 1 downto 0 do
      FBits^[i] := FBits^[i] / f;
end;

procedure TMorphomatics.ADD_(bin: TMorphologyBinaryzation; K: TMorphomaticsValue);
var
  i, j: Integer;
begin
  for j := 0 to umlMin(Height, bin.Height) - 1 do
    for i := 0 to umlMin(Width, bin.Width) - 1 do
      if bin[i, j] then
          Pixel[i, j] := Pixel[i, j] + K;
end;

procedure TMorphomatics.SUB_(bin: TMorphologyBinaryzation; K: TMorphomaticsValue);
var
  i, j: Integer;
begin
  for j := 0 to umlMin(Height, bin.Height) - 1 do
    for i := 0 to umlMin(Width, bin.Width) - 1 do
      if bin[i, j] then
          Pixel[i, j] := Pixel[i, j] - K;
end;

procedure TMorphomatics.MUL_(bin: TMorphologyBinaryzation; K: TMorphomaticsValue);
var
  i, j: Integer;
begin
  for j := 0 to umlMin(Height, bin.Height) - 1 do
    for i := 0 to umlMin(Width, bin.Width) - 1 do
      if bin[i, j] then
          Pixel[i, j] := Pixel[i, j] * K;
end;

procedure TMorphomatics.DIV_(bin: TMorphologyBinaryzation; K: TMorphomaticsValue);
var
  i, j: Integer;
begin
  for j := 0 to umlMin(Height, bin.Height) - 1 do
    for i := 0 to umlMin(Width, bin.Width) - 1 do
      if bin[i, j] then
          Pixel[i, j] := Pixel[i, j] / K;
end;

procedure TMorphomatics.Dilatation(ConvolutionKernel: TMorphologyBinaryzation; output: TMorphomatics);
var
  kH, kW, kOffsetY, kModY, kOffsetX, kModX: Integer;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(sY: Integer);
  var
    sX, aY, aX, kY, kX: Integer;
    LMax: TMorphomaticsValue;
  begin
    for sX := 0 to Width - 1 do
      begin
        LMax := MinSingle;

        kY := 0;
        for aY := sY - kOffsetY to kH - kOffsetY + sY + kModY do
          begin
            kX := 0;
            for aX := sX - kOffsetX to kW - kOffsetX + sX + kModX do
              begin
                if (aY >= 0) and (aY < Height) and (aX >= 0) and (aX < Width) and (ConvolutionKernel[kX, kY]) then
                    LMax := umlMax(LMax, Pixel[aX, aY]);
                inc(kX);
              end;
            inc(kY);
          end;

        output[sX, sY] := LMax;
      end;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    sY: Integer;
    sX, aY, aX, kY, kX: Integer;
    LMax: TMorphomaticsValue;
  begin
    for sY := 0 to Height - 1 do
      for sX := 0 to Width - 1 do
        begin
          LMax := MinSingle;

          kY := 0;
          for aY := sY - kOffsetY to kH - kOffsetY + sY + kModY do
            begin
              kX := 0;
              for aX := sX - kOffsetX to kW - kOffsetX + sX + kModX do
                begin
                  if (aY >= 0) and (aY < Height) and (aX >= 0) and (aX < Width) and (ConvolutionKernel[kX, kY]) then
                      LMax := umlMax(LMax, Pixel[aX, aY]);
                  inc(kX);
                end;
              inc(kY);
            end;

          output[sX, sY] := LMax;
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

  output.SetSize(Width, Height);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMorphomatics.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMorphomatics.Parallel and LocalParallel, 0, Height - 1, procedure(sY: Integer)
    var
      sX, aY, aX, kY, kX: Integer;
      LMax: TMorphomaticsValue;
    begin
      for sX := 0 to Width - 1 do
        begin
          LMax := MinSingle;

          kY := 0;
          for aY := sY - kOffsetY to kH - kOffsetY + sY + kModY do
            begin
              kX := 0;
              for aX := sX - kOffsetX to kW - kOffsetX + sX + kModX do
                begin
                  if (aY >= 0) and (aY < Height) and (aX >= 0) and (aX < Width) and (ConvolutionKernel[kX, kY]) then
                      LMax := umlMax(LMax, Pixel[aX, aY]);
                  inc(kX);
                end;
              inc(kY);
            end;

          output[sX, sY] := LMax;
        end;
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
end;

procedure TMorphomatics.Erosion(ConvolutionKernel: TMorphologyBinaryzation; output: TMorphomatics);
var
  kH, kW, kOffsetY, kModY, kOffsetX, kModX: Integer;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(sY: Integer);
  var
    sX, aY, aX, kY, kX: Integer;
    LMin: TMorphomaticsValue;
  begin
    for sX := 0 to Width - 1 do
      begin
        LMin := MaxSingle;

        kY := 0;
        for aY := sY - kOffsetY to kH - kOffsetY + sY + kModY do
          begin
            kX := 0;
            for aX := sX - kOffsetX to kW - kOffsetX + sX + kModX do
              begin
                if (aY >= 0) and (aY < Height) and (aX >= 0) and (aX < Width) and (ConvolutionKernel[kX, kY]) then
                    LMin := umlMin(LMin, Pixel[aX, aY]);
                inc(kX);
              end;
            inc(kY);
          end;

        output[sX, sY] := LMin;
      end;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    sY: Integer;
    sX, aY, aX, kY, kX: Integer;
    LMin: TMorphomaticsValue;
  begin
    for sY := 0 to Height - 1 do
      for sX := 0 to Width - 1 do
        begin
          LMin := MaxSingle;

          kY := 0;
          for aY := sY - kOffsetY to kH - kOffsetY + sY + kModY do
            begin
              kX := 0;
              for aX := sX - kOffsetX to kW - kOffsetX + sX + kModX do
                begin
                  if (aY >= 0) and (aY < Height) and (aX >= 0) and (aX < Width) and (ConvolutionKernel[kX, kY]) then
                      LMin := umlMin(LMin, Pixel[aX, aY]);
                  inc(kX);
                end;
              inc(kY);
            end;

          output[sX, sY] := LMin;
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

  output.SetSize(Width, Height);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMorphomatics.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMorphomatics.Parallel and LocalParallel, 0, Height - 1, procedure(sY: Integer)
    var
      sX, aY, aX, kY, kX: Integer;
      LMin: TMorphomaticsValue;
    begin
      for sX := 0 to Width - 1 do
        begin
          LMin := MaxSingle;

          kY := 0;
          for aY := sY - kOffsetY to kH - kOffsetY + sY + kModY do
            begin
              kX := 0;
              for aX := sX - kOffsetX to kW - kOffsetX + sX + kModX do
                begin
                  if (aY >= 0) and (aY < Height) and (aX >= 0) and (aX < Width) and (ConvolutionKernel[kX, kY]) then
                      LMin := umlMin(LMin, Pixel[aX, aY]);
                  inc(kX);
                end;
              inc(kY);
            end;

          output[sX, sY] := LMin;
        end;
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
end;

procedure TMorphomatics.Opening(ConvolutionKernel: TMorphologyBinaryzation; output: TMorphomatics);
begin
  Erosion(ConvolutionKernel, output);
  output.Dilatation(ConvolutionKernel);
end;

procedure TMorphomatics.Closing(ConvolutionKernel: TMorphologyBinaryzation; output: TMorphomatics);
begin
  Dilatation(ConvolutionKernel, output);
  output.Erosion(ConvolutionKernel);
end;

procedure TMorphomatics.OpeningAndClosing(ConvolutionKernel: TMorphologyBinaryzation; output: TMorphomatics);
begin
  Opening(ConvolutionKernel, output);
  output.Closing(ConvolutionKernel);
end;

procedure TMorphomatics.ClosingAndOpening(ConvolutionKernel: TMorphologyBinaryzation; output: TMorphomatics);
begin
  Closing(ConvolutionKernel, output);
  output.Opening(ConvolutionKernel);
end;

procedure TMorphomatics.Dilatation(ConvolutionKernel: TMorphologyBinaryzation);
var
  tmp: TMorphomatics;
begin
  tmp := TMorphomatics.Create;
  Dilatation(ConvolutionKernel, tmp);
  SwapData(tmp);
  DisposeObject(tmp);
end;

procedure TMorphomatics.Erosion(ConvolutionKernel: TMorphologyBinaryzation);
var
  tmp: TMorphomatics;
begin
  tmp := TMorphomatics.Create;
  Erosion(ConvolutionKernel, tmp);
  SwapData(tmp);
  DisposeObject(tmp);
end;

procedure TMorphomatics.Opening(ConvolutionKernel: TMorphologyBinaryzation);
begin
  Erosion(ConvolutionKernel);
  Dilatation(ConvolutionKernel);
end;

procedure TMorphomatics.Closing(ConvolutionKernel: TMorphologyBinaryzation);
begin
  Dilatation(ConvolutionKernel);
  Erosion(ConvolutionKernel);
end;

procedure TMorphomatics.OpeningAndClosing(ConvolutionKernel: TMorphologyBinaryzation);
begin
  Opening(ConvolutionKernel);
  Closing(ConvolutionKernel);
end;

procedure TMorphomatics.ClosingAndOpening(ConvolutionKernel: TMorphologyBinaryzation);
begin
  Closing(ConvolutionKernel);
  Opening(ConvolutionKernel);
end;

procedure TMorphomatics.Dilatation(const ConvolutionSizeX, ConvolutionSizeY: Integer; output: TMorphomatics);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  Dilatation(tmp, output);
  DisposeObject(tmp);
end;

procedure TMorphomatics.Erosion(const ConvolutionSizeX, ConvolutionSizeY: Integer; output: TMorphomatics);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  Erosion(tmp, output);
  DisposeObject(tmp);
end;

procedure TMorphomatics.Opening(const ConvolutionSizeX, ConvolutionSizeY: Integer; output: TMorphomatics);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  Opening(tmp, output);
  DisposeObject(tmp);
end;

procedure TMorphomatics.Closing(const ConvolutionSizeX, ConvolutionSizeY: Integer; output: TMorphomatics);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  Closing(tmp, output);
  DisposeObject(tmp);
end;

procedure TMorphomatics.OpeningAndClosing(const ConvolutionSizeX, ConvolutionSizeY: Integer; output: TMorphomatics);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  OpeningAndClosing(tmp, output);
  DisposeObject(tmp);
end;

procedure TMorphomatics.ClosingAndOpening(const ConvolutionSizeX, ConvolutionSizeY: Integer; output: TMorphomatics);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  ClosingAndOpening(tmp, output);
  DisposeObject(tmp);
end;

procedure TMorphomatics.Dilatation(const ConvolutionSizeX, ConvolutionSizeY: Integer);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  Dilatation(tmp);
  DisposeObject(tmp);
end;

procedure TMorphomatics.Erosion(const ConvolutionSizeX, ConvolutionSizeY: Integer);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  Erosion(tmp);
  DisposeObject(tmp);
end;

procedure TMorphomatics.Opening(const ConvolutionSizeX, ConvolutionSizeY: Integer);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  Opening(tmp);
  DisposeObject(tmp);
end;

procedure TMorphomatics.Closing(const ConvolutionSizeX, ConvolutionSizeY: Integer);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  Closing(tmp);
  DisposeObject(tmp);
end;

procedure TMorphomatics.OpeningAndClosing(const ConvolutionSizeX, ConvolutionSizeY: Integer);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  OpeningAndClosing(tmp);
  DisposeObject(tmp);
end;

procedure TMorphomatics.ClosingAndOpening(const ConvolutionSizeX, ConvolutionSizeY: Integer);
var
  tmp: TMorphologyBinaryzation;
begin
  tmp := TMorphologyBinaryzation.Create;
  tmp.SetConvolutionSize(ConvolutionSizeX, ConvolutionSizeY, True);
  ClosingAndOpening(tmp);
  DisposeObject(tmp);
end;

function TMorphomatics.Binarization(Thresold: TMorphomaticsValue): TMorphologyBinaryzation;
var
  i: Integer;
begin
  Result := TMorphologyBinaryzation.Create;
  Result.LocalParallel := LocalParallel;
  Result.SetSize(Width, Height);
  for i := Width * Height - 1 downto 0 do
      Result.FBits^[i] := FBits^[i] >= Thresold;
end;

function TMorphomatics.Binarization_InRange(Min_, Max_: TMorphomaticsValue): TMorphologyBinaryzation;
var
  i: Integer;
begin
  Result := TMorphologyBinaryzation.Create;
  Result.LocalParallel := LocalParallel;
  Result.SetSize(Width, Height);
  for i := Width * Height - 1 downto 0 do
      Result.FBits^[i] := umlInRange(FBits^[i], Min_, Max_);
end;

function TMorphomatics.Binarization_Bernsen(R: Integer; ContrastThresold: TMorphomaticsValue): TMorphologyBinaryzation;
var
  output_: TMorphologyBinaryzation;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  var
    X, internali, internalj: Integer;
    IMin, IMax, IAvg, localContrast, v: TMorphomaticsValue;
  begin
    for X := R to Width - 1 - R do
      begin
        IMin := 1;
        IMax := 0;
        for internali := pass - R to pass + R do
          for internalj := X - R to X + R do
            begin
              v := Pixel[internalj, internali];
              if v > IMax then
                  IMax := v;
              if v < IMin then
                  IMin := v;
            end;
        localContrast := IMax - IMin;
        IAvg := (IMax - IMin) * 0.5;
        if localContrast < ContrastThresold then
            output_[X, pass] := IAvg >= 0.5
        else
            output_[X, pass] := Pixel[X, pass] >= IAvg;
      end;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    pass: Integer;
    X, internali, internalj: Integer;
    IMin, IMax, IAvg, localContrast, v: TMorphomaticsValue;
  begin
    for pass := R to Height - 1 - R do
      begin
        for X := R to Width - 1 - R do
          begin
            IMin := 1;
            IMax := 0;
            for internali := pass - R to pass + R do
              for internalj := X - R to X + R do
                begin
                  v := Pixel[internalj, internali];
                  if v > IMax then
                      IMax := v;
                  if v < IMin then
                      IMin := v;
                end;
            localContrast := IMax - IMin;
            IAvg := (IMax - IMin) * 0.5;
            if localContrast < ContrastThresold then
                output_[X, pass] := IAvg >= 0.5
            else
                output_[X, pass] := Pixel[X, pass] >= IAvg;
          end;
      end;
  end;
{$ENDIF Parallel}


begin
  output_ := TMorphologyBinaryzation.Create;
  output_.LocalParallel := LocalParallel;
  output_.SetSize(Width, Height, False);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMorphomatics.Parallel and LocalParallel, @Nested_ParallelFor, R, Height - 1 - R);
{$ELSE FPC}
  DelphiParallelFor(TMorphomatics.Parallel and LocalParallel, R, Height - 1 - R, procedure(pass: Integer)
    var
      X, internali, internalj: Integer;
      IMin, IMax, IAvg, localContrast, v: TMorphomaticsValue;
    begin
      for X := R to Width - 1 - R do
        begin
          IMin := 1;
          IMax := 0;
          for internali := pass - R to pass + R do
            for internalj := X - R to X + R do
              begin
                v := Pixel[internalj, internali];
                if v > IMax then
                    IMax := v;
                if v < IMin then
                    IMin := v;
              end;
          localContrast := IMax - IMin;
          IAvg := (IMax - IMin) * 0.5;
          if localContrast < ContrastThresold then
              output_[X, pass] := IAvg >= 0.5
          else
              output_[X, pass] := Pixel[X, pass] >= IAvg;
        end;
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  Result := output_;
end;

function TMorphomatics.Binarization_FloydSteinbergDithering: TMorphologyBinaryzation;
var
  Err: TMorphomaticsMatrix;
  d: TMorphomaticsValue;
  Y, X: Integer;
begin
  Result := TMorphologyBinaryzation.Create;
  Result.LocalParallel := LocalParallel;
  Result.SetSize(Width, Height);

  SetLength(Err, Height, Width);
  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
        Err[Y, X] := 0;

  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
      begin
        if Pixel[X, Y] + Err[Y, X] > 0.5 then
          begin
            Result[X, Y] := False;
            d := -(1 - Pixel[X, Y]);
          end
        else
          begin
            Result[X, Y] := True;
            d := Pixel[X, Y]
          end;
        if X + 1 <= Width - 1 then
            Err[Y, X + 1] := 7 * d / 16;
        if (Y + 1 <= Height - 1) and (X - 1 >= 0) then
            Err[Y + 1, X - 1] := 3 * d / 16;
        if (Y + 1 <= Height - 1) then
            Err[Y + 1, X] := 5 * d / 16;
        if (Y + 1 <= Height - 1) and (X + 1 <= Width - 1) then
            Err[Y + 1, X + 1] := 1 * d / 16;
      end;
  SetLength(Err, 0, 0);
end;

function TMorphomatics.Binarization_OTSU: TMorphologyBinaryzation;
var
  Histogram: array [0 .. $FF] of TMorphomaticsValue;
  level, IMax, IMin, i, j, num: Integer;
  Gray: Byte;
  Mean, Variance, Mu, Omega, LevelMean, LargestMu: TMorphomaticsValue;
begin
  FillPtrByte(@Histogram[0], SizeOf(Histogram), 0);
  IMin := $FF;
  IMax := 0;
  level := 0;
  num := Width * Height;

  // Compute histogram and determine IMin and IMax pixel values
  for i := 0 to num - 1 do
    begin
      Gray := ClampByte(Round(Bits^[i] * $FF));
      Histogram[Gray] := Histogram[Gray] + 1.0;
      if Gray < IMin then
          IMin := Gray;
      if Gray > IMax then
          IMax := Gray;
    end;

  // Normalize histogram
  for i := 0 to $FF do
      Histogram[i] := Histogram[i] / num;

  // Compute image mean and variance
  Mean := 0.0;
  Variance := 0.0;
  for i := 0 to $FF do
      Mean := Mean + (i + 1) * Histogram[i];
  for i := 0 to $FF do
      Variance := Variance + Sqr(i + 1 - Mean) * Histogram[i];

  // Now finally compute threshold level
  LargestMu := 0;

  for i := 0 to $FF do
    begin
      Omega := 0.0;
      LevelMean := 0.0;
      for j := 0 to i - 1 do
        begin
          Omega := Omega + Histogram[j];
          LevelMean := LevelMean + (j + 1) * Histogram[j];
        end;
      Mu := Sqr(Mean * Omega - LevelMean);
      Omega := Omega * (1.0 - Omega);
      if Omega > 0.0 then
          Mu := Mu / Omega
      else
          Mu := 0;
      if Mu > LargestMu then
        begin
          LargestMu := Mu;
          level := i;
        end;
    end;

  Result := TMorphologyBinaryzation.Create;
  Result.LocalParallel := LocalParallel;
  Result.SetSize(Width, Height);
  for i := 0 to Width * Height - 1 do
      Result.Bits^[i] := Round(Bits^[i] * $FF) >= level;
end;

class procedure TMorphomatics.Test();
var
  ori, tmp: TMorphomatics;
  m64: TMemoryStream64;
  md5_1, md5_2: TMD5;
begin
  ori := TMorphomatics.Create;
  ori.SetSize(1920, 1080);
  ori.FillRandomValue;

  tmp := TMorphomatics.Create;

  DoStatusNoLn('TESTING Morphomatics SAVESTREAM..');
  m64 := TMemoryStream64.Create;
  ori.SaveToStream(m64);
  md5_1 := umlStreamMD5(m64);
  m64.Position := 0;
  DoStatusNoLn('TESTING Morphomatics LOADSTREAM..');
  tmp.LoadFromStream(m64);
  m64.Clear;
  tmp.SaveToStream(m64);
  md5_2 := umlStreamMD5(m64);
  if umlMD5Compare(md5_1, md5_2) then
      DoStatus('STREAM Morphomatics VERIFY OK')
  else
      DoStatus('STREAM Morphomatics VERIFY FAILED');
  DisposeObject(m64);
  DoStatusNoLn;

  DoStatusNoLn('TESTING AVGFilter..');
  tmp.Assign(ori);
  tmp.Average(5, 5);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING WeightedAVG..');
  tmp.Assign(ori);
  tmp.WeightedAVG(5, 5);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING GeometricMean..');
  tmp.Assign(ori);
  tmp.GeometricMean(5, 5);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Median..');
  tmp.Assign(ori);
  tmp.Median(5, 5);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Max..');
  tmp.Assign(ori);
  tmp.Maximum(5, 5);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Min..');
  tmp.Assign(ori);
  tmp.Minimum(5, 5);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING MiddlePoint..');
  tmp.Assign(ori);
  tmp.MiddlePoint(5, 5);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING TruncatedAVG..');
  tmp.Assign(ori);
  tmp.TruncatedAVG(5, 5, 2);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Previtt..');
  tmp.Assign(ori);
  tmp.Previtt(True);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Sobel..');
  tmp.Assign(ori);
  tmp.Sobel(True);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Sharr..');
  tmp.Assign(ori);
  tmp.Sharr(True);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Laplace..');
  tmp.Assign(ori);
  tmp.Laplace(True);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Linear..');
  tmp.Assign(ori);
  tmp.Linear(0.1, 1);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Log..');
  tmp.Assign(ori);
  tmp.Logarithms(0.5);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Gamma..');
  tmp.Assign(ori);
  tmp.Gamma(0.1, 0.8);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING HistogramEqualization..');
  tmp.Assign(ori);
  tmp.HistogramEqualization;

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Contrast..');
  tmp.Assign(ori);
  tmp.Contrast(0.9);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Clamp..');
  tmp.Assign(ori);
  tmp.Clamp(0, 1);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING ADD..');
  tmp.Assign(ori);
  tmp.ADD_(ori);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING SUB..');
  tmp.Assign(ori);
  tmp.SUB_(ori);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING MUL..');
  tmp.Assign(ori);
  tmp.MUL_(ori);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING DIV..');
  tmp.Assign(ori);
  tmp.DIV_(ori);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Dilatation..');
  tmp.Assign(ori);
  tmp.Dilatation(Bin9x9);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Erosion..');
  tmp.Assign(ori);
  tmp.Erosion(Bin9x9);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Opening..');
  tmp.Assign(ori);
  tmp.Opening(Bin9x9);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Closing..');
  tmp.Assign(ori);
  tmp.Closing(Bin9x9);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING OpeningAndClosing..');
  tmp.Assign(ori);
  tmp.OpeningAndClosing(Bin9x9);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING ClosingAndOpening..');
  tmp.Assign(ori);
  tmp.ClosingAndOpening(Bin9x9);

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Binarization..');
  tmp.Assign(ori);
  DisposeObject(tmp.Binarization(0.5));

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Binarization_InRange..');
  tmp.Assign(ori);
  DisposeObject(tmp.Binarization_InRange(0.0, 1.0));

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Binarization_Bernsen..');
  tmp.Assign(ori);
  DisposeObject(tmp.Binarization_Bernsen(5, 0.8));

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Binarization_FloydSteinbergDithering..');
  tmp.Assign(ori);
  DisposeObject(tmp.Binarization_FloydSteinbergDithering());

  DoStatusNoLn('DONE');
  DoStatusNoLn;
  DoStatusNoLn('TESTING Binarization_OTSU..');
  tmp.Assign(ori);
  DisposeObject(tmp.Binarization_OTSU());

  DoStatusNoLn;

  DisposeObject(ori);
  DisposeObject(tmp);
  DoStatus('All Morphomatics Test done.');
end;
