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

constructor TSegmentationGeometry.Create;
begin
  inherited Create;
end;

destructor TSegmentationGeometry.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TSegmentationGeometry.InsertGeo(index: Integer; X, Y: Integer; segPtr: PMorphologySegData);
var
  p: PMorphologyGeoData;
begin
  new(p);
  p^.X := X;
  p^.Y := Y;
  p^.segPtr := segPtr;
  inherited Insert(index, p);
end;

procedure TSegmentationGeometry.AddGeo(X, Y: Integer; segPtr: PMorphologySegData);
var
  p: PMorphologyGeoData;
begin
  new(p);
  p^.X := X;
  p^.Y := Y;
  p^.segPtr := segPtr;
  inherited Add(p);
end;

procedure TSegmentationGeometry.Remove(p: PMorphologyGeoData);
begin
  Dispose(p);
  inherited Remove(p);
end;

procedure TSegmentationGeometry.Delete(index: TGeoInt);
begin
  if (index >= 0) and (index < Count) then
    begin
      Dispose(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TSegmentationGeometry.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Dispose(Items[i]);
  inherited Clear;
end;

constructor TMorphologyPool.Create;
begin
  inherited Create;
  FBoundsCached := False;
  FBoundsCache := NullRectV2;
  FPixelSumCache := 0;
  FClassify := 0;
  FGroupID := -1;
  Owner := nil;
end;

procedure TMorphologyPool.AddSeg(const buff: array of PMorphologySegData);
var
  f: array of Boolean;
  i, j: Integer;
  p: PMorphologySegData;
begin
  SetLength(f, length(buff));
  for j := 0 to length(f) - 1 do
      f[j] := False;
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      for j := 0 to length(f) - 1 do
        if (p = buff[j]) and (buff[j] <> nil) then
            f[j] := True;
    end;
  for j := 0 to length(f) - 1 do
    if (not f[j]) and (buff[j] <> nil) then
        Add(buff[j]);
  SetLength(f, 0);
end;

procedure TMorphologyPool.SortY;

  function Sort_Compare(const Item1, Item2: PMorphologySegData): Integer;
  begin
    if Item1^.Y < Item2^.Y then
        Result := -1
    else if Item1^.Y > Item2^.Y then
        Result := 1
    else if Item1^.L < Item2^.L then
        Result := -1
    else if Item1^.L > Item2^.L then
        Result := 1
    else
        Result := 0;
  end;

  procedure QuickSortList(iLo, iHi: Integer);
  var
    Lo, Hi, Mid: Integer;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := (Lo + Hi) div 2;
    repeat
      while Sort_Compare(Items[Lo], Items[Mid]) < 0 do
          inc(Lo);
      while Sort_Compare(Items[Hi], Items[Mid]) > 0 do
          dec(Hi);
      if Lo <= Hi then
        begin
          Exchange(Lo, Hi);
          if Mid = Lo then
              Mid := Hi
          else if Mid = Hi then
              Mid := Lo;
          inc(Lo);
          dec(Hi);
        end;
    until Lo > Hi;

    if Hi > iLo then
        QuickSortList(iLo, Hi);

    if Lo < iHi then
        QuickSortList(Lo, iHi);
  end;

begin
  if Count > 1 then
      QuickSortList(0, Count - 1);
end;

function TMorphologyPool.BoundsRectV2(cache: Boolean): TRectV2;
var
  i: Integer;
  p: PMorphologySegData;
begin
  if FBoundsCached and cache then
    begin
      Result := FBoundsCache;
      exit;
    end;

  if Count > 0 then
    begin
      p := First;
      Result[0] := Vec2(p^.L, p^.Y);
      Result[1] := Result[0];
      for i := 0 to Count - 1 do
        begin
          p := Items[i];
          Result := BoundRect(Result, RectV2(p^.L, p^.Y, p^.R, p^.Y));
        end;

      Result := CalibrationRectInRect(RectEdge(ForwardRect(Result), 1), Owner.BoundsRectV20);

      FBoundsCache := Result;
      FBoundsCached := True;
    end
  else
      Result := NullRectV2;
end;

function TMorphologyPool.BoundsRectV2: TRectV2;
begin
  Result := BoundsRectV2(True);
end;

function TMorphologyPool.BoundsRect: TRect;
begin
  Result := Rect2Rect(BoundsRectV2);
end;

function TMorphologyPool.Centre: TVec2;
begin
  Result := RectCentre(BoundsRectV2);
end;

function TMorphologyPool.Left: Integer;
begin
  Result := BoundsRect.Left;
end;

function TMorphologyPool.Top: Integer;
begin
  Result := BoundsRect.Top;
end;

function TMorphologyPool.Width: Integer;
begin
  Result := Round(RectWidth(BoundsRectV2)) + 1;
end;

function TMorphologyPool.Height: Integer;
begin
  Result := Round(RectHeight(BoundsRectV2)) + 1;
end;

function TMorphologyPool.PixelSum: Integer;
var
  i: Integer;
  p: PMorphologySegData;
begin
  if FPixelSumCache <= 0 then
    begin
      FPixelSumCache := 0;
      for i := 0 to Count - 1 do
        begin
          p := Items[i];
          inc(FPixelSumCache, p^.R - p^.L);
        end;
    end;
  Result := FPixelSumCache;
end;

function TMorphologyPool.Area: Integer;
begin
  Result := Round(RectArea(BoundsRectV2));
end;

function TMorphologyPool.BuildBinaryzation(): TMorphologyBinaryzation;
var
  i, j: Integer;
  p: PMorphologySegData;
begin
  Result := TMorphologyBinaryzation.Create;
  Result.SetSize(Owner.Width, Owner.Height, False);
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      for j := p^.L to p^.R do
          Result[j, p^.Y] := True;
    end;
end;

procedure TMorphologyPool.FillToBinaryzation(morphBin_: TMorphologyBinaryzation);
var
  i, j: Integer;
  p: PMorphologySegData;
begin
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      for j := p^.L to p^.R do
          morphBin_[j, p^.Y] := True;
    end;
end;

procedure TMorphologyPool.DrawTo(dest: TMemoryRaster; DataColor: TRColor);
var
  i, j: Integer;
  p: PMorphologySegData;
begin
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      if p^.Y < dest.Height then
        for j := p^.L to p^.R do
          if j < dest.Width then
              dest[j, p^.Y] := DataColor;
    end;
end;

procedure TMorphologyPool.ProjectionTo(Source, dest: TMemoryRaster);
var
  i, j: Integer;
  p: PMorphologySegData;
begin
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      for j := p^.L to p^.R do
          dest[j, p^.Y] := Source[j, p^.Y];
    end;
end;

function TMorphologyPool.Projection(Source: TMemoryRaster): TMemoryRaster;
var
  i, j: Integer;
begin
  Result := NewRaster();
  Result.SetSize(Owner.Width, Owner.Height, RColor(0, 0, 0, 0));
  ProjectionTo(Source, Result);
end;

function TMorphologyPool.BuildDatamap(backColor, DataColor: TRColor): TMemoryRaster;
begin
  Result := NewRaster();
  Result.SetSize(Owner.Width, Owner.Height, backColor);
  DrawTo(Result, DataColor);
end;

function TMorphologyPool.BuildClipDatamap(backColor, DataColor: TRColor): TMemoryRaster;
var
  R: TRect;
  i, j: Integer;
  p: PMorphologySegData;
  pc: PRColor;
begin
  R := BoundsRect;
  Result := NewRaster();
  Result.SetSizeF(R.Right - R.Left + 1, R.Bottom - R.Top + 1, backColor);
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      pc := Result.PixelPtr[p^.L - R.Left, p^.Y - R.Top];
      for j := p^.L to p^.R do
        begin
          pc^ := DataColor;
          inc(pc);
        end;
    end;
end;

function TMorphologyPool.BuildClipMap(Source: TMemoryRaster; backColor: TRColor): TMemoryRaster;
var
  R: TRect;
  i, j: Integer;
  p: PMorphologySegData;
  ps, pd: PRColor;
begin
  if (Source.Width <> Owner.Width) or (Source.Height <> Owner.Height) then
    begin
      Result := nil;
      exit;
    end;

  R := BoundsRect;
  Result := NewRaster();
  Result.SetSizeF(R.Right - R.Left + 1, R.Bottom - R.Top + 1, backColor);
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      ps := Source.PixelPtr[p^.L, p^.Y];
      pd := Result.PixelPtr[p^.L - R.Left, p^.Y - R.Top];
      for j := p^.L to p^.R do
        begin
          pd^ := ps^;
          inc(ps^);
          inc(pd);
        end;
    end;
end;

function TMorphologyPool.BuildVertex(): T2DPolygon;
var
  i, j: Integer;
  segPtr: PMorphologySegData;
begin
  Result := nil;
  if Count = 0 then
      exit;

  // combine fragment
  Result := T2DPolygon.Create;
  for i := 0 to Count - 1 do
    begin
      segPtr := Items[i];
      if segPtr^.L <> segPtr^.R then
        begin
          Result.Add(segPtr^.L, segPtr^.Y);
          Result.Add(segPtr^.R, segPtr^.Y);
        end
      else
          Result.Add(segPtr^.L, segPtr^.Y);
    end;
  Result.RemoveSame;
end;

function TMorphologyPool.BuildConvexHull(): T2DPolygon;
begin
  Result := BuildVertex();
  if Result <> nil then
      Result.ConvexHull();
end;

function TMorphologyPool.BuildLines(Reduction: TGeoFloat): TLinesList;
var
  FragObjs: TCoreClassObjectList;
  cID: TMorphologyClassify;
  i, j, K: Integer;
  segPtr: PMorphologySegData;
  geo: TSegmentationGeometry;
begin
  Result := nil;
  if Count = 0 then
      exit;

  FragObjs := TCoreClassObjectList.Create(True);
  cID := FClassify;

  for i := 0 to Count - 1 do
    begin
      segPtr := Items[i];
      segPtr^.LGeometry := nil;
      segPtr^.RGeometry := nil;
    end;

  // left fragment
  for i := 0 to Count - 1 do
    begin
      segPtr := Items[i];
      if segPtr^.LGeometry = nil then
        begin
          geo := TSegmentationGeometry.Create;
          FragObjs.Add(geo);
          segPtr^.LGeometry := geo;
          segPtr^.LGeometry.AddGeo(segPtr^.L, segPtr^.Y, segPtr);
          while (segPtr^.LBot <> nil) and (segPtr^.LBot^.Classify = cID) and (segPtr^.LBot^.GroupID = FGroupID) and
            (segPtr^.LBot^.LGeometry = nil) and (segPtr^.LBot^.LTop = segPtr) do
            begin
              segPtr^.LBot^.LGeometry := segPtr^.LGeometry;
              segPtr := segPtr^.LBot;
              segPtr^.LGeometry.AddGeo(segPtr^.L, segPtr^.Y, segPtr);
            end;
        end;
    end;

  // right fragment
  for i := 0 to Count - 1 do
    begin
      segPtr := Items[i];
      if segPtr^.RGeometry = nil then
        begin
          geo := TSegmentationGeometry.Create;
          FragObjs.Add(geo);
          segPtr^.RGeometry := geo;
          segPtr^.RGeometry.AddGeo(segPtr^.R, segPtr^.Y, segPtr);
          while (segPtr^.RBot <> nil) and (segPtr^.RBot^.Classify = cID) and (segPtr^.RBot^.GroupID = FGroupID) and
            (segPtr^.RBot^.RGeometry = nil) and (segPtr^.RBot^.RTop = segPtr) do
            begin
              segPtr^.RBot^.RGeometry := segPtr^.RGeometry;
              segPtr := segPtr^.RBot;
              segPtr^.RGeometry.AddGeo(segPtr^.R, segPtr^.Y, segPtr);
            end;
        end;
    end;

  // extract fragment
  Result := TLinesList.Create;
  for i := 0 to FragObjs.Count - 1 do
    begin
      geo := TSegmentationGeometry(FragObjs[i]);
      Result.Add(TLines.Create);

      if geo.Count > 1 then
        begin
          // fill polygon
          for j := 0 to geo.Count - 1 do
            with geo[j]^ do
                Result.Last.Add(X, Y);

          // Douglas¨CPeucker optimization
          if Reduction > 0 then
              Result.Last.Reduction(Reduction);
        end;
    end;

  // reset geometry link
  for i := 0 to Count - 1 do
    begin
      segPtr := Items[i];
      segPtr^.LGeometry := nil;
      segPtr^.RGeometry := nil;
    end;

  disposeObject(FragObjs);
end;

function TMorphologyPool.IsGroup(const X, Y: Integer): Boolean;
begin
  if (X >= 0) and (X < Owner.Width) and (Y >= 0) and (Y < Owner.Height) then
      Result := (Owner.FSegMap[Y, X] <> nil) and (Owner.FSegMap[Y, X]^.GroupID = FGroupID)
  else
      Result := False;
end;

function TMorphologyPool.IsEdge(const X, Y: Integer): Boolean;
begin
  Result := True;
  if IsGroup(X, Y) then
    begin
      // inside
      if (Y <= 0) or (not IsGroup(X, Y - 1)) then
          exit;
      if (Y >= Owner.Height) or (not IsGroup(X, Y + 1)) then
          exit;
      if (X <= 0) or (not IsGroup(X - 1, Y)) then
          exit;
      if (X >= Owner.Width) or (not IsGroup(X + 1, Y)) then
          exit;
      if (not IsGroup(X - 1, Y - 1)) then
          exit;
      if (not IsGroup(X + 1, Y - 1)) then
          exit;
      if (not IsGroup(X - 1, Y + 1)) then
          exit;
      if (not IsGroup(X + 1, Y + 1)) then
          exit;
    end
  else
    begin
      // outside
      if (Y > 0) and (IsGroup(X, Y - 1)) then
          exit;
      if (Y < Owner.Height) and (IsGroup(X, Y + 1)) then
          exit;
      if (X > 0) and (IsGroup(X - 1, Y)) then
          exit;
      if (X < Owner.Width) and (IsGroup(X + 1, Y)) then
          exit;
      if (IsGroup(X - 1, Y - 1)) then
          exit;
      if (IsGroup(X + 1, Y - 1)) then
          exit;
      if (IsGroup(X - 1, Y + 1)) then
          exit;
      if (IsGroup(X + 1, Y + 1)) then
          exit;
    end;
  Result := False;
end;

function TMorphologyPool.ScanEdge(const Y, L, R: Integer): Boolean;
var
  i, LL, RR: Integer;
begin
  Result := False;
  LL := L;
  RR := R;
  if LL > RR then
      Swap(LL, RR);
  for i := LL to RR do
    if not IsEdge(i, Y) then
        exit;
  Result := True;
end;

function TMorphologyPool.BuildGeometry(Reduction: TGeoFloat): T2DPolygonGraph;

  function CombineDecision(output_geo: TSegmentationGeometry; p: PMorphologySegData; var destGeo: TSegmentationGeometry; var destSeg: PMorphologySegData): Boolean;
  var
    X: Integer;
    PrevS, NextS: Byte;
  begin
    Result := False;
    if output_geo.Last^.segPtr = p then
        X := output_geo.Last^.X
    else if output_geo.First^.segPtr = p then
        X := output_geo.First^.X
    else
        raiseInfo('error');

    if (p^.LGeometry <> nil) and (p^.LGeometry <> output_geo) and
      ((p^.LGeometry.First^.segPtr = p) or (p^.LGeometry.Last^.segPtr = p)) and (ScanEdge(p^.Y, X, p^.L)) then
      begin
        destGeo := p^.LGeometry;
        destSeg := p;
        Result := True;
        exit;
      end;

    if (p^.RGeometry <> nil) and (p^.RGeometry <> output_geo) and
      ((p^.RGeometry.First^.segPtr = p) or (p^.RGeometry.Last^.segPtr = p)) and (ScanEdge(p^.Y, X, p^.R)) then
      begin
        destGeo := p^.RGeometry;
        destSeg := p;
        Result := True;
        exit;
      end;

    PrevS := 0;
    NextS := 0;
    if (p^.Left <> nil) and (p^.Left^.GroupID = FGroupID) and (p^.Left^.Classify = FClassify) and
      (p^.Left^.RGeometry <> nil) and (p^.Left^.RGeometry <> output_geo) and (ScanEdge(p^.Y, X, p^.Left^.R)) and
      ((p^.Left^.RGeometry.First^.segPtr = p^.Left) or (p^.Left^.RGeometry.Last^.segPtr = p^.Left)) then
      begin
        inc(PrevS);
        if (p^.Left^.LGeometry <> nil) and (p^.Left^.LGeometry <> output_geo) then
            inc(PrevS);
      end;

    if (p^.Right <> nil) and (p^.Right^.GroupID = FGroupID) and (p^.Right^.Classify = FClassify) and
      (p^.Right^.LGeometry <> nil) and (p^.Right^.LGeometry <> output_geo) and (ScanEdge(p^.Y, X, p^.Right^.L)) and
      ((p^.Right^.LGeometry.First^.segPtr = p^.Right) or (p^.Right^.LGeometry.Last^.segPtr = p^.Right)) then
      begin
        inc(NextS);
        if (p^.Right^.RGeometry <> nil) and (p^.Right^.RGeometry <> output_geo) then
            inc(NextS);
      end;

    if (NextS > 0) and (NextS > PrevS) then
      begin
        destGeo := p^.Right^.LGeometry;
        destSeg := p^.Right;
        Result := True;
        exit;
      end;
    if (PrevS > 0) then
      begin
        destGeo := p^.Left^.RGeometry;
        destSeg := p^.Left;
        Result := True;
        exit;
      end;
  end;

  procedure RemoveGeoInfo(OldGeo, NewGeo: TSegmentationGeometry);
  var
    i: Integer;
    p: PMorphologySegData;
  begin
    for i := 0 to Count - 1 do
      begin
        p := Items[i];
        if p^.LGeometry = OldGeo then
            p^.LGeometry := NewGeo;
        if p^.RGeometry = OldGeo then
            p^.RGeometry := NewGeo;
      end;
  end;

  procedure CombineGeometryToSour(sourGeo: TSegmentationGeometry; sourSeg: PMorphologySegData; destGeo: TSegmentationGeometry; destSeg: PMorphologySegData);
  var
    i: Integer;
  begin
    if (sourGeo.First^.segPtr = sourSeg) and (destGeo.First^.segPtr = destSeg) then
      begin
        for i := destGeo.Count - 1 downto 0 do
          with destGeo[i]^ do
              sourGeo.InsertGeo(0, X, Y, segPtr);
        RemoveGeoInfo(destGeo, sourGeo);
      end
    else if (sourGeo.Last^.segPtr = sourSeg) and (destGeo.First^.segPtr = destSeg) then
      begin
        for i := 0 to destGeo.Count - 1 do
          with destGeo[i]^ do
              sourGeo.AddGeo(X, Y, segPtr);
        RemoveGeoInfo(destGeo, sourGeo);
      end
    else if (sourGeo.First^.segPtr = sourSeg) and (destGeo.Last^.segPtr = destSeg) then
      begin
        for i := 0 to destGeo.Count - 1 do
          with destGeo[i]^ do
              sourGeo.InsertGeo(0, X, Y, segPtr);
        RemoveGeoInfo(destGeo, sourGeo);
      end
    else if (sourGeo.Last^.segPtr = sourSeg) and (destGeo.Last^.segPtr = destSeg) then
      begin
        for i := destGeo.Count - 1 downto 0 do
          with destGeo[i]^ do
              sourGeo.AddGeo(X, Y, segPtr);
        RemoveGeoInfo(destGeo, sourGeo);
      end
    else
      begin
        raiseInfo('error');
      end;
  end;

  procedure CombineGeometry(sour_geo, geo_output: TSegmentationGeometry);
  var
    i: Integer;
    destGeo: TSegmentationGeometry;
    destSeg: PMorphologySegData;
  begin
    for i := 0 to sour_geo.Count - 1 do
      with sour_geo[i]^ do
          geo_output.AddGeo(X, Y, segPtr);
    RemoveGeoInfo(sour_geo, geo_output);

    while CombineDecision(geo_output, geo_output.Last^.segPtr, destGeo, destSeg) do
        CombineGeometryToSour(geo_output, geo_output.Last^.segPtr, destGeo, destSeg);

    while CombineDecision(geo_output, geo_output.First^.segPtr, destGeo, destSeg) do
        CombineGeometryToSour(geo_output, geo_output.First^.segPtr, destGeo, destSeg);
  end;

var
  FragObjs, combineObjs: TCoreClassObjectList;
  cID: TMorphologyClassify;
  i, j, K: Integer;
  segPtr: PMorphologySegData;
  geo, tmp: TSegmentationGeometry;
  Polygon: T2DPolygon;
  IsCollapse: Boolean;
begin
  Result := nil;
  if Count = 0 then
      exit;

  FragObjs := TCoreClassObjectList.Create(True);
  combineObjs := TCoreClassObjectList.Create(True);
  cID := FClassify;

  for i := 0 to Count - 1 do
    begin
      segPtr := Items[i];
      segPtr^.LGeometry := nil;
      segPtr^.RGeometry := nil;
    end;

  // left fragment
  for i := 0 to Count - 1 do
    begin
      segPtr := Items[i];
      if segPtr^.LGeometry = nil then
        begin
          geo := TSegmentationGeometry.Create;
          FragObjs.Add(geo);
          segPtr^.LGeometry := geo;
          segPtr^.LGeometry.AddGeo(segPtr^.L, segPtr^.Y, segPtr);
          while (segPtr^.LBot <> nil) and (segPtr^.LBot^.Classify = cID) and (segPtr^.LBot^.GroupID = FGroupID) and
            (segPtr^.LBot^.LGeometry = nil) and (segPtr^.LBot^.LTop = segPtr) do
            begin
              segPtr^.LBot^.LGeometry := segPtr^.LGeometry;
              segPtr := segPtr^.LBot;
              segPtr^.LGeometry.AddGeo(segPtr^.L, segPtr^.Y, segPtr);
            end;
        end;
    end;

  // right fragment
  for i := 0 to Count - 1 do
    begin
      segPtr := Items[i];
      if segPtr^.RGeometry = nil then
        begin
          geo := TSegmentationGeometry.Create;
          FragObjs.Add(geo);
          segPtr^.RGeometry := geo;
          segPtr^.RGeometry.AddGeo(segPtr^.R, segPtr^.Y, segPtr);
          while (segPtr^.RBot <> nil) and (segPtr^.RBot^.Classify = cID) and (segPtr^.RBot^.GroupID = FGroupID) and
            (segPtr^.RBot^.RGeometry = nil) and (segPtr^.RBot^.RTop = segPtr) do
            begin
              segPtr^.RBot^.RGeometry := segPtr^.RGeometry;
              segPtr := segPtr^.RBot;
              segPtr^.RGeometry.AddGeo(segPtr^.R, segPtr^.Y, segPtr);
            end;
        end;
    end;

  // combine fragment
  Result := T2DPolygonGraph.Create;
  if FragObjs.Count > 0 then
    begin
      // build Surround
      geo := FragObjs.First as TSegmentationGeometry;
      tmp := TSegmentationGeometry.Create;
      combineObjs.Add(tmp);
      CombineGeometry(geo, tmp);
      Polygon := Result.Surround;

      // fill surround polygon
      for i := 0 to tmp.Count - 1 do
        with tmp[i]^ do
            Polygon.Add(X, Y);

      // Douglas¨CPeucker optimization
      if Reduction > 0 then
          Polygon.Reduction(Reduction)
      else
          Polygon.RemoveSame;

      // build Collapses
      for j := 1 to FragObjs.Count - 1 do
        begin
          geo := FragObjs[j] as TSegmentationGeometry;
          if ((geo.First^.segPtr^.LGeometry = geo) or (geo.First^.segPtr^.RGeometry = geo)) and
            ((geo.Last^.segPtr^.LGeometry = geo) or (geo.Last^.segPtr^.RGeometry = geo)) then
            begin
              tmp := TSegmentationGeometry.Create;
              combineObjs.Add(tmp);
              CombineGeometry(geo, tmp);

              // check collapse
              IsCollapse := True;
              for i := 0 to tmp.Count - 1 do
                with tmp[i]^ do
                  if not Result.Surround.InHere(Vec2(X, Y)) then
                    begin
                      IsCollapse := False;
                      break;
                    end;

              if IsCollapse then
                begin
                  Polygon := Result.NewCollapse();

                  // fill Collapses polygon
                  for i := 0 to tmp.Count - 1 do
                    with tmp[i]^ do
                        Polygon.Add(X, Y);

                  // Douglas¨CPeucker optimization
                  if Reduction > 0 then
                      Polygon.Reduction(Reduction)
                  else
                      Polygon.RemoveSame;
                end;
            end;
        end;
    end;

  // reset geometry link
  for i := 0 to Count - 1 do
    begin
      segPtr := Items[i];
      segPtr^.LGeometry := nil;
      segPtr^.RGeometry := nil;
    end;

  disposeObject(FragObjs);
  disposeObject(combineObjs);
end;

function TMorphologyPool.BuildConvolutionGeometry(Reduction: TGeoFloat;
  Operation_: TBinaryzationOperation; ConvolutionKernel: TMorphologyBinaryzation): T2DPolygonGraph;
var
  tmp_bin: TMorphologyBinaryzation;
  tmp_seg: TMorphologySegmentation;
begin
  tmp_bin := BuildBinaryzation();
  tmp_bin.Process(Operation_, ConvolutionKernel);
  tmp_seg := tmp_bin.BuildMorphologySegmentation();
  disposeObject(tmp_bin);

  if tmp_seg.Count > 0 then
      Result := tmp_seg[0].BuildGeometry(Reduction)
  else
      Result := BuildGeometry(Reduction);

  disposeObject(tmp_seg);
end;

function TMorphologyPool.BuildConvolutionGeometry(Reduction: TGeoFloat): T2DPolygonGraph;
begin
  Result := BuildConvolutionGeometry(Reduction, boNone, nil);
end;

function TMorphologySegmentation.NewMorphologySegData(X, Y: Integer; Classify: TMorphologyClassify): PMorphologySegData;
begin
  new(Result);
  Result^.Y := Y;
  Result^.L := X;
  Result^.R := X;
  Result^.LTop := nil;
  Result^.RTop := nil;
  Result^.LBot := nil;
  Result^.RBot := nil;
  Result^.Left := nil;
  Result^.Right := nil;
  Result^.GroupID := -1;
  Result^.Classify := Classify;
  Result^.LGeometry := nil;
  Result^.RGeometry := nil;
  Result^.index := FSource.Count;
  FSource.Add(Result);
  FSegMap[Y, X] := Result;
end;

function TMorphologySegmentation.FindPool(p: PMorphologySegData): TMorphologyPool;
var
  i, j: Integer;
  sp: TMorphologyPool;
  p2: PMorphologySegData;
begin
  Result := nil;
  if p <> nil then
    for j := 0 to FMorphologyPoolList.Count - 1 do
      begin
        sp := FMorphologyPoolList[j];
        if (sp.FClassify = p^.Classify) then // optimized find
          for i := sp.Count - 1 downto 0 do
            begin
              p2 := sp[i];
              if (p2 = p)
                or ((p^.LTop <> nil) and (p2 = p^.LTop))
                or ((p^.RTop <> nil) and (p2 = p^.RTop))
                or ((p^.LBot <> nil) and (p2 = p^.LBot))
                or ((p^.RBot <> nil) and (p2 = p^.RBot)) then
                begin
                  Result := sp;
                  exit;
                end;
            end;
      end;
end;

function TMorphologySegmentation.GetOrCreatePool(p: PMorphologySegData): TMorphologyPool;
begin
  Result := FindPool(p);
  if Result = nil then
    begin
      Result := TMorphologyPool.Create;
      Result.Owner := Self;
      Result.FClassify := p^.Classify;
      FMorphologyPoolList.Add(Result);
    end;
end;

procedure TMorphologySegmentation.AddToGroup(p: PMorphologySegData);
var
  nPool: TMorphologyPool;
begin
  if p = nil then
      exit;
  nPool := GetOrCreatePool(p);
  nPool.AddSeg([p, p^.LTop, p^.RTop, p^.LBot, p^.RBot]);
end;

function TMorphologySegmentation.GetPools(X, Y: Integer): TMorphologyPool;
var
  p: PMorphologySegData;
begin
  Result := nil;
  p := FSegMap[Y, X];
  if p = nil then
      exit;
  Result := FMorphologyPoolList[p^.GroupID];
end;

function TMorphologySegmentation.GetItems(index: Integer): TMorphologyPool;
begin
  Result := FMorphologyPoolList[index];
end;

procedure TMorphologySegmentation.ResetSource;
var
  i: Integer;
begin
  for i := 0 to FSource.Count - 1 do
      Dispose(FSource[i]);
  FSource.Clear;
end;

procedure TMorphologySegmentation.PrepareMap(Width_, Height_: Integer);
var
  i, j: Integer;
begin
  Clear();

  // init Map param
  SetLength(FSegMap, Height_, Width_);
  FWidth := Width_;
  FHeight := Height_;
  // fill Map value
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
        FSegMap[j, i] := nil;
end;

procedure TMorphologySegmentation.InternalFillMap(var classifyMap_: TMorphologySegClassifyMap; Width_, Height_: Integer);
var
  Classify_, L_id: TMorphologyClassify;
  i, j: Integer;
  p, prevSeg: PMorphologySegData;
  L, R: PMorphologySegData;
begin
  // line segmentation
  for j := 0 to FHeight - 1 do
    begin
      L_id := 0;
      p := nil;
      for i := 0 to FWidth - 1 do
        begin
          // combine near classify
          Classify_ := classifyMap_[j, i];

          // lines seg
          if Classify_ > 0 then
            begin
              if (i > 0) and (L_id = Classify_) then
                begin
                  p := FSegMap[j, i - 1];
                  p^.R := i;
                  FSegMap[j, i] := p;
                end
              else
                begin
                  prevSeg := p;
                  p := NewMorphologySegData(i, j, Classify_);
                  if prevSeg <> nil then
                    begin
                      prevSeg^.Right := p;
                      p^.Left := prevSeg;
                    end;
                end;
            end;

          L_id := Classify_;
        end;
    end;

  // horz link
  for j := 0 to FSource.Count - 1 do
    begin
      p := FSource[j];
      for i := p^.L to p^.R do
        begin
          if (p^.Y > 0) and (FSegMap[p^.Y - 1, i] <> nil) and (p^.Classify = FSegMap[p^.Y - 1, i]^.Classify) then
            begin
              p^.RTop := FSegMap[p^.Y - 1, i];
              if p^.LTop = nil then
                  p^.LTop := p^.RTop;
            end;
          if (p^.Y + 1 < FHeight) and (FSegMap[p^.Y + 1, i] <> nil) and (p^.Classify = FSegMap[p^.Y + 1, i]^.Classify) then
            begin
              p^.RBot := FSegMap[p^.Y + 1, i];
              if p^.LBot = nil then
                  p^.LBot := p^.RBot;
            end;
        end;
    end;
end;

procedure TMorphologySegmentation.ExtractSegLinkToGroup();
var
  i: Integer;
begin
  for i := 0 to FSource.Count - 1 do
      AddToGroup(FSource[i]);
end;

constructor TMorphologySegmentation.Create();
var
  i, j: Integer;
begin
  inherited Create;

  FWidth := 0;
  FHeight := 0;
  SetLength(FSegMap, 0, 0);
  FSource := TMorphologyPool_Decl.Create;
  FMorphologyPoolList := TMorphologyPoolList.Create;
  FOnGetPixelSegClassify := nil;
  FOnGetMorphomaticsSegClassify := nil;
end;

destructor TMorphologySegmentation.Destroy;
var
  i: Integer;
begin
  SetLength(FSegMap, 0, 0);
  ResetSource();
  disposeObject(FSource);
  for i := 0 to FMorphologyPoolList.Count - 1 do
      disposeObject(FMorphologyPoolList[i]);
  disposeObject(FMorphologyPoolList);

  inherited Destroy;
end;

procedure TMorphologySegmentation.Clear();
var
  i: Integer;
begin
  // reset source
  ResetSource();

  // reset Morphology Pool
  for i := 0 to FMorphologyPoolList.Count - 1 do
      disposeObject(FMorphologyPoolList[i]);
  FMorphologyPoolList.Clear;

  // reset Map param
  SetLength(FSegMap, 0, 0);
  FWidth := 0;
  FHeight := 0;
end;

function TMorphologySegmentation.DoGetPixelSegClassify(X, Y: Integer; Color: TRColor): TMorphologyClassify;
begin
  Result := 0;
  if Assigned(FOnGetPixelSegClassify) then
      FOnGetPixelSegClassify(X, Y, Color, Result)
  else if Color = $FFFFFFFF then
      Result := 0
  else
      Result := Round(Trunc(RColor2Gray(Color) / ($FF / 16)) * ($FF / 16));
end;

procedure TMorphologySegmentation.BuildSegmentation(raster: TMemoryRaster);
var
  tmp_classifyMap: TMorphologySegClassifyMap;
  i, j: Integer;
begin
  SetLength(tmp_classifyMap, raster.Height, raster.Width);
  for j := 0 to raster.Height - 1 do
    for i := 0 to raster.Width - 1 do
        tmp_classifyMap[j, i] := DoGetPixelSegClassify(i, j, raster[i, j]);
  BuildSegmentation(tmp_classifyMap, raster.Width, raster.Height);
  SetLength(tmp_classifyMap, 0, 0);
end;

procedure TMorphologySegmentation.BuildSegmentation(raster: TMemoryRaster;
  ConvolutionOperations: array of TBinaryzationOperation; ConvWidth, ConvHeight, MaxClassifyCount, MinGranularity: Integer);
var
  tmp_classifyMap: TMorphologySegClassifyMap;
  i, j: Integer;
  tmpBin: TMorphologyBinaryzation;
begin
  SetLength(tmp_classifyMap, raster.Height, raster.Width);
  for j := 0 to raster.Height - 1 do
    for i := 0 to raster.Width - 1 do
        tmp_classifyMap[j, i] := DoGetPixelSegClassify(i, j, raster[i, j]);

  tmpBin := TMorphologyBinaryzation.Create;
  tmpBin.SetConvolutionSize(ConvWidth, ConvHeight, True);
  ClassifyMapConvolution(raster.Width, raster.Height, tmp_classifyMap, ConvolutionOperations, tmpBin, MaxClassifyCount, MinGranularity);
  disposeObject(tmpBin);

  BuildSegmentation(tmp_classifyMap, raster.Width, raster.Height);
  SetLength(tmp_classifyMap, 0, 0);
end;

function TMorphologySegmentation.DoGetMorphomaticsSegClassify(X, Y: Integer; Morph: TMorphomaticsValue): TMorphologyClassify;
begin
  Result := 0;
  if Assigned(FOnGetMorphomaticsSegClassify) then
      FOnGetMorphomaticsSegClassify(X, Y, Morph, Result)
  else
      Result := Trunc(Trunc(Trunc(umlClamp(Morph, 0.0, 1.0) * $FF) / ($FF / 16)) * ($FF / 16));
end;

procedure TMorphologySegmentation.BuildSegmentation(Morph: TMorphomatics);
var
  tmp_classifyMap: TMorphologySegClassifyMap;
  i, j: Integer;
begin
  SetLength(tmp_classifyMap, Morph.Height, Morph.Width);
  for j := 0 to Morph.Height - 1 do
    for i := 0 to Morph.Width - 1 do
        tmp_classifyMap[j, i] := DoGetMorphomaticsSegClassify(i, j, Morph[i, j]);
  BuildSegmentation(tmp_classifyMap, Morph.Width, Morph.Height);
  SetLength(tmp_classifyMap, 0, 0);
end;

procedure TMorphologySegmentation.BuildSegmentation(Binaryzation: TMorphologyBinaryzation);
var
  tmp_classifyMap: TMorphologySegClassifyMap;
  i, j: Integer;
begin
  SetLength(tmp_classifyMap, Binaryzation.Height, Binaryzation.Width);
  for j := 0 to Binaryzation.Height - 1 do
    for i := 0 to Binaryzation.Width - 1 do
      if Binaryzation[i, j] then
          tmp_classifyMap[j, i] := 1
      else
          tmp_classifyMap[j, i] := 0;
  BuildSegmentation(tmp_classifyMap, Binaryzation.Width, Binaryzation.Height);
  SetLength(tmp_classifyMap, 0, 0);
end;

procedure TMorphologySegmentation.BuildSegmentation(var classifyMap_: TMorphologySegClassifyMap; Width_, Height_: Integer);
begin
  ClassifyMapFillVacancy(Width_, Height_, classifyMap_);
  PrepareMap(Width_, Height_);
  InternalFillMap(classifyMap_, Width_, Height_);
  ExtractSegLinkToGroup();
  UpdateMorphologyPool();
end;

function TMorphologySegmentation.GetClassifyMap: TMorphologySegClassifyMap;
var
  i, j: Integer;
begin
  SetLength(Result, FHeight, FWidth);
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
      if FSegMap[j, i] <> nil then
          Result[j, i] := FSegMap[j, i]^.Classify
      else
          Result[j, i] := 0;
end;

procedure TMorphologySegmentation.UpdateMorphologyPool();

  function ComparePool(const Item1, Item2: TMorphologyPool): ShortInt;
  begin
    if Item1.PixelSum < Item2.PixelSum then
        Result := 1
    else if Item1.PixelSum > Item2.PixelSum then
        Result := -1
    else
        Result := 0;
  end;

  procedure SortPool(iLo, iHi: Integer);
  var
    Lo, Hi, Mid: Integer;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := (Lo + Hi) div 2;
    repeat
      while ComparePool(FMorphologyPoolList[Lo], FMorphologyPoolList[Mid]) < 0 do
          inc(Lo);
      while ComparePool(FMorphologyPoolList[Hi], FMorphologyPoolList[Mid]) > 0 do
          dec(Hi);
      if Lo <= Hi then
        begin
          FMorphologyPoolList.Exchange(Lo, Hi);
          if Mid = Lo then
              Mid := Hi
          else if Mid = Hi then
              Mid := Lo;
          inc(Lo);
          dec(Hi);
        end;
    until Lo > Hi;

    if Hi > iLo then
        SortPool(iLo, Hi);
    if Lo < iHi then
        SortPool(Lo, iHi);
  end;

var
  i, j: Integer;
  p: PMorphologySegData;
  sp, found_sp: TMorphologyPool;
  re_sp: Boolean;
begin
  // optimized combine
  j := 0;
  while j < FMorphologyPoolList.Count do
    begin
      sp := FMorphologyPoolList[j];
      repeat
        re_sp := False;
        i := 0;
        while i < sp.Count do
          begin
            p := sp[i];
            found_sp := FindPool(p);
            if found_sp <> sp then
              begin
                // combine to found_sp
                if found_sp = nil then
                  begin
                    found_sp := TMorphologyPool.Create;
                    found_sp.Owner := Self;
                    found_sp.FClassify := p^.Classify;
                    FMorphologyPoolList.Add(found_sp);
                  end;
                found_sp.AddSeg([p, p^.LTop, p^.RTop, p^.LBot, p^.RBot]);

                sp.Delete(i);
                re_sp := True;
              end
            else
                inc(i);
          end;
      until not re_sp;

      if sp.Count > 0 then
          inc(j)
      else
        begin
          FMorphologyPoolList.Delete(j);
          disposeObject(sp);
        end;
    end;

  // sort Y
  for j := 0 to FMorphologyPoolList.Count - 1 do
    begin
      sp := FMorphologyPoolList[j];
      sp.FBoundsCached := False;
      sp.FPixelSumCache := 0;
      sp.SortY;
    end;

  // sort pool
  if FMorphologyPoolList.Count > 1 then
      SortPool(0, FMorphologyPoolList.Count - 1);

  // reset group ID
  for j := 0 to FMorphologyPoolList.Count - 1 do
    begin
      sp := FMorphologyPoolList[j];
      sp.FGroupID := j + 1;
      for i := 0 to sp.Count - 1 do
        begin
          sp[i]^.GroupID := sp.FGroupID;
          sp[i]^.Classify := sp.Classify;
        end;
    end;
end;

procedure TMorphologySegmentation.MergeOverlapSegmentation();
  function MergeToPool(pool: TMorphologyPool): Boolean;
  var
    r1, r2: TRectV2;
    i, j: Integer;
    rect_overlap: Boolean;
    p: PMorphologySegData;
    sp: TMorphologyPool;
  begin
    Result := False;
    r1 := pool.BoundsRectV2(False);
    i := 0;
    for i := 0 to FMorphologyPoolList.Count - 1 do
      begin
        if (FMorphologyPoolList[i] <> pool) and (FMorphologyPoolList[i].FClassify = pool.FClassify) then
          begin
            sp := FMorphologyPoolList[i];
            r2 := sp.BoundsRectV2(False);
            rect_overlap := RectWithinRect(r1, r2) or RectWithinRect(r2, r1) or RectToRectIntersect(r1, r2) or RectToRectIntersect(r2, r1);
            if rect_overlap then
              begin
                // merge to pool
                for j := 0 to sp.Count - 1 do
                  begin
                    p := sp[j];
                    pool.AddSeg([p, p^.LTop, p^.RTop, p^.LBot, p^.RBot]);
                  end;
                disposeObject(sp);
                FMorphologyPoolList.Delete(i);
                MergeToPool(pool);
                Result := True;
                exit;
              end;
          end;
      end;
  end;

var
  i: Integer;
begin
  i := 0;
  while i < FMorphologyPoolList.Count do
    if MergeToPool(FMorphologyPoolList[i]) then
        i := 0
    else
        inc(i);

  UpdateMorphologyPool;
end;

function TMorphologySegmentation.RemoveNoise(PixelNoiseThreshold: Integer): Boolean;
var
  i, j: Integer;
  sp: TMorphologyPool;
  p: PMorphologySegData;
begin
  Result := False;
  i := 0;
  while i < FMorphologyPoolList.Count do
    if FMorphologyPoolList[i].PixelSum < PixelNoiseThreshold then
      begin
        sp := FMorphologyPoolList[i];
        for j := 0 to sp.Count - 1 do
          begin
            p := sp[j];
            p^.GroupID := -1;
            p^.Classify := 0;
          end;
        disposeObject(FMorphologyPoolList[i]);
        FMorphologyPoolList.Delete(i);
        Result := True;
      end
    else
        inc(i);

  if Result then
      UpdateMorphologyPool;
end;

function TMorphologySegmentation.Clone: TMorphologySegmentation;
begin
  Result := TMorphologySegmentation.Create;
  Result.Assign(Self);
end;

procedure TMorphologySegmentation.Assign(sour: TMorphologySegmentation);
var
  i, j: Integer;
  p1: PMorphologySegData;
  nPool, sPool: TMorphologyPool;
begin
  // reset
  ResetSource();
  for i := 0 to FMorphologyPoolList.Count - 1 do
      disposeObject(FMorphologyPoolList[i]);
  FMorphologyPoolList.Clear;

  SetLength(FSegMap, sour.Height, sour.Width);
  FWidth := sour.Width;
  FHeight := sour.Height;
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
        FSegMap[j, i] := nil;

  // copy source
  FSource.Capacity := sour.FSource.Count;
  for i := 0 to sour.FSource.Count - 1 do
    begin
      new(p1);
      p1^ := sour.FSource[i]^;
      FSource.Add(p1);
    end;
  // rebuild data link
  for j := 0 to FSource.Count - 1 do
    begin
      p1 := FSource[j];
      if p1^.LTop <> nil then
          p1^.LTop := FSource[p1^.LTop^.index];
      if p1^.RTop <> nil then
          p1^.RTop := FSource[p1^.RTop^.index];
      if p1^.LBot <> nil then
          p1^.LBot := FSource[p1^.LBot^.index];
      if p1^.RBot <> nil then
          p1^.RBot := FSource[p1^.RBot^.index];
      if p1^.Left <> nil then
          p1^.Left := FSource[p1^.Left^.index];
      if p1^.Right <> nil then
          p1^.Right := FSource[p1^.Right^.index];

      for i := p1^.L to p1^.R do
          FSegMap[p1^.Y, i] := p1;
    end;
  // copy pool
  for j := 0 to sour.FMorphologyPoolList.Count - 1 do
    begin
      sPool := sour.FMorphologyPoolList[j];

      nPool := TMorphologyPool.Create;
      nPool.FClassify := sPool.FClassify;
      nPool.FGroupID := sPool.FGroupID;
      nPool.Owner := Self;
      FMorphologyPoolList.Add(nPool);

      nPool.Capacity := sPool.Count;
      for i := 0 to sPool.Count - 1 do
          nPool.Add(FSource[sPool[i]^.index]);
    end;
end;

procedure TMorphologySegmentation.SaveToStream(stream: TCoreClassStream);
  procedure WriteSegData(segPtr: PMorphologySegData; saveTo: TMemoryStream64);
    procedure WriteIndex_(p: PMorphologySegData);
    begin
      if p <> nil then
          saveTo.WriteInt32(p^.index)
      else
          saveTo.WriteInt32(-1);
    end;

  begin
    saveTo.WriteInt32(segPtr^.Y);
    saveTo.WriteInt32(segPtr^.L);
    saveTo.WriteInt32(segPtr^.R);
    WriteIndex_(segPtr^.LTop);
    WriteIndex_(segPtr^.RTop);
    WriteIndex_(segPtr^.LBot);
    WriteIndex_(segPtr^.RBot);
    WriteIndex_(segPtr^.Left);
    WriteIndex_(segPtr^.Right);
    saveTo.WriteInt32(segPtr^.GroupID);
    saveTo.WriteUInt32(segPtr^.Classify);
    saveTo.WriteInt32(segPtr^.index);
  end;

  procedure WritePoolData(Pool_: TMorphologyPool; saveTo: TMemoryStream64);
  var
    i: Integer;
  begin
    saveTo.WriteUInt32(Pool_.FClassify);
    saveTo.WriteInt32(Pool_.FGroupID);
    saveTo.WriteInt32(Pool_.Count);
    for i := 0 to Pool_.Count - 1 do
        saveTo.WriteInt32(Pool_[i]^.index);
  end;

var
  i: Integer;
  de: TDataFrameEngine;
  tmp: TMemoryStream64;
begin
  de := TDataFrameEngine.Create;
  // write size info
  de.WriteInteger(FWidth);
  de.WriteInteger(FHeight);

  // write seg data
  de.WriteInteger(FSource.Count);
  tmp := TMemoryStream64.CustomCreate(8192);
  for i := 0 to FSource.Count - 1 do
      WriteSegData(FSource[i], tmp);
  de.WriteStream(tmp);
  disposeObject(tmp);

  // write pool data
  for i := 0 to FMorphologyPoolList.Count - 1 do
    begin
      tmp := TMemoryStream64.CustomCreate(8192);
      WritePoolData(FMorphologyPoolList[i], tmp);
      de.WriteStream(tmp);
      disposeObject(tmp);
    end;

  de.EncodeAsZLib(stream);
  disposeObject(de);
end;

procedure TMorphologySegmentation.LoadFromStream(stream: TCoreClassStream);
  procedure ReadSegData(sour: TMemoryStream64; total_: Integer);
    function ReadIndex_: PMorphologySegData;
    var
      tmp: Integer;
    begin
      tmp := sour.ReadInt32;
      if tmp >= 0 then
          Result := FSource[tmp]
      else
          Result := nil;
    end;

  var
    i, j: Integer;
    p: PMorphologySegData;
  begin
    FSource.Capacity := total_;
    for i := 0 to total_ - 1 do
      begin
        new(p);
        p^.Y := 0;
        p^.L := 0;
        p^.R := 0;
        p^.LTop := nil;
        p^.RTop := nil;
        p^.LBot := nil;
        p^.RBot := nil;
        p^.Left := nil;
        p^.Right := nil;
        p^.GroupID := -1;
        p^.Classify := 0;
        p^.LGeometry := nil;
        p^.RGeometry := nil;
        p^.index := i;
        FSource.Add(p);
      end;
    for j := 0 to FSource.Count - 1 do
      begin
        p := FSource[j];
        p^.Y := sour.ReadInt32;
        p^.L := sour.ReadInt32;
        p^.R := sour.ReadInt32;
        p^.LTop := ReadIndex_;
        p^.RTop := ReadIndex_;
        p^.LBot := ReadIndex_;
        p^.RBot := ReadIndex_;
        p^.Left := ReadIndex_;
        p^.Right := ReadIndex_;

        p^.GroupID := sour.ReadInt32;
        p^.Classify := sour.ReadUInt32;
        p^.index := sour.ReadInt32;

        for i := p^.L to p^.R do
            FSegMap[p^.Y, i] := p;
      end;
  end;

  procedure ReadPoolData(sour: TMemoryStream64);
  var
    Pool_: TMorphologyPool;
    total_: Integer;
    i: Integer;
  begin
    Pool_ := TMorphologyPool.Create;
    Pool_.Owner := Self;
    FMorphologyPoolList.Add(Pool_);

    Pool_.FClassify := sour.ReadUInt32;
    Pool_.FGroupID := sour.ReadInt32;
    total_ := sour.ReadInt32;

    Pool_.Capacity := total_;
    for i := 0 to total_ - 1 do
        Pool_.Add(FSource[sour.ReadInt32]);
  end;

var
  i, j: Integer;
  de: TDataFrameEngine;
  tmp: TMemoryStream64;
  total_: Integer;
begin
  // reset source
  ResetSource();
  for i := 0 to FMorphologyPoolList.Count - 1 do
      disposeObject(FMorphologyPoolList[i]);
  // reset pool
  FMorphologyPoolList.Clear;

  // extract stream as TDataFrameEngine
  de := TDataFrameEngine.Create;
  de.DecodeFrom(stream);

  // reset map
  FWidth := de.Reader.ReadInteger;
  FHeight := de.Reader.ReadInteger;
  SetLength(FSegMap, FHeight, FWidth);
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
        FSegMap[j, i] := nil;

  // read seg
  total_ := de.Reader.ReadInteger;
  tmp := TMemoryStream64.Create;
  de.Reader.ReadStream(tmp);
  tmp.Position := 0;
  ReadSegData(tmp, total_);
  disposeObject(tmp);

  // read pool
  FMorphologyPoolList.Capacity := de.Count - de.Reader.index;
  while de.Reader.NotEnd do
    begin
      tmp := TMemoryStream64.Create;
      de.Reader.ReadStream(tmp);
      tmp.Position := 0;
      ReadPoolData(tmp);
      disposeObject(tmp);
    end;

  disposeObject(de);
end;

function TMorphologySegmentation.BuildBinaryzation(): TMorphologyBinaryzation;
var
  i: Integer;
begin
  Result := TMorphologyBinaryzation.Create;
  Result.SetSize(Width, Height, False);
  for i := 0 to FMorphologyPoolList.Count - 1 do
      FMorphologyPoolList[i].FillToBinaryzation(Result);
end;

function TMorphologySegmentation.Projection(Source: TMemoryRaster): TMemoryRaster;
var
  i: Integer;
begin
  Result := NewRaster();
  Result.SetSize(Width, Height, RColor(0, 0, 0, 0));
  for i := 0 to FMorphologyPoolList.Count - 1 do
      FMorphologyPoolList[i].ProjectionTo(Source, Result);
end;

function TMorphologySegmentation.BuildViewer(): TMemoryRaster;
var
  i: Integer;
  Pool_: TMorphologyPool;
  C: TRColor;
begin
  Result := NewRaster();
  Result.SetSize(Width, Height, RColor(0, 0, 0, 0));
  Result.OpenAgg;
  for i := 0 to FMorphologyPoolList.Count - 1 do
    begin
      Pool_ := FMorphologyPoolList[i];
      C := RandomRColor($FF);
      Pool_.DrawTo(Result, C);
      Result.DrawRect(Pool_.BoundsRectV2, RColor($FF, $FF, $FF));
    end;
end;

function TMorphologySegmentation.Count: Integer;
begin
  Result := FMorphologyPoolList.Count;
end;

function TMorphologySegmentation.Width0: Integer;
begin
  if FWidth > 0 then
      Result := FWidth - 1
  else
      Result := 0;
end;

function TMorphologySegmentation.Height0: Integer;
begin
  if FHeight > 0 then
      Result := FHeight - 1
  else
      Result := 0;
end;

function TMorphologySegmentation.SizeOfPoint: TPoint;
begin
  Result := Point(Width, Height);
end;

function TMorphologySegmentation.SizeOf2DPoint: TVec2;
begin
  Result := Vec2(Width, Height);
end;

function TMorphologySegmentation.Size2D: TVec2;
begin
  Result := Vec2(Width, Height);
end;

function TMorphologySegmentation.Size0: TVec2;
begin
  Result := Vec2(Width0, Height0);
end;

function TMorphologySegmentation.BoundsRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

function TMorphologySegmentation.BoundsRect0: TRect;
begin
  Result := Rect(0, 0, Width0, Height0);
end;

function TMorphologySegmentation.BoundsRectV2: TRectV2;
begin
  Result := RectV2(0, 0, Width, Height);
end;

function TMorphologySegmentation.BoundsRectV20: TRectV2;
begin
  Result := RectV2(0, 0, Width0, Height0);
end;

function TMorphologySegmentation.BoundsV2Rect4: TV2Rect4;
begin
  Result := TV2Rect4.Init(BoundsRectV2);
end;

function TMorphologySegmentation.BoundsV2Rect40: TV2Rect4;
begin
  Result := TV2Rect4.Init(BoundsRectV20);
end;

function TMorphologySegmentation.Centroid: TVec2;
begin
  Result := RectCentre(BoundsRectV2);
end;

function TMorphologySegmentation.Centre: TVec2;
begin
  Result := RectCentre(BoundsRectV2);
end;

function TMorphologySegmentation.InHere(const X, Y: Integer): Boolean;
begin
  Result := PointInRect(X, Y, 0, 0, Width0, Height0);
end;

class procedure TMorphologySegmentation.Test(inputfile, outputfile: SystemString);
var
  R: TMemoryRaster;
  colors: array of TRColor;
  i, j: Integer;
  sp: TMorphologyPool;
begin
  R := NewRasterFromFile(inputfile);

  with TMorphologySegmentation.Create() do
    begin
      BuildSegmentation(R);
      R.Clear(RColor(0, 0, 0));
      SetLength(colors, FMorphologyPoolList.Count);
      for i := 0 to FMorphologyPoolList.Count - 1 do
          colors[i] := RandomRColor;

      for j := 0 to Count - 1 do
        begin
          sp := Items[j];
          sp.DrawTo(R, colors[j]);
        end;
      Free;
    end;
  R.SaveToFile(outputfile);
  disposeObject(R);
end;

procedure ClassifyMapFillVacancy(Width, Height: Integer; var classifyMap: TMorphologySegClassifyMap);
var
  tmpSeg: TMorphologySegmentation;
  i, j: Integer;
  p: PMorphologySegData;
  done: Boolean;
  Completed: Integer;
begin
  repeat
    Completed := 0;
    tmpSeg := TMorphologySegmentation.Create;
    tmpSeg.PrepareMap(Width, Height);
    tmpSeg.InternalFillMap(classifyMap, Width, Height);
    for j := 0 to tmpSeg.FSource.Count - 1 do
      begin
        p := tmpSeg.FSource[j];
        done := False;
        if (not done) and (p^.Right <> nil) and (p^.Classify = p^.Right^.Classify) then
          begin
            if ((p^.LBot = nil) and (p^.RBot = nil) and (p^.RTop <> nil) and (p^.RTop = p^.Right^.LTop)) or
              ((p^.LTop = nil) and (p^.RTop = nil) and (p^.RBot <> nil) and (p^.RBot = p^.Right^.LBot)) then
              begin
                inc(Completed);
                for i := p^.R to p^.Right^.L do
                    classifyMap[p^.Y, i] := p^.Classify;
                done := True;
              end;
          end;

        if (not done) and (p^.Left <> nil) and (p^.Classify = p^.Left^.Classify) then
          begin
            if ((p^.RBot = nil) and (p^.LBot = nil) and (p^.LTop <> nil) and (p^.LTop = p^.Left^.RTop)) or
              ((p^.RTop = nil) and (p^.LTop = nil) and (p^.LBot <> nil) and (p^.LBot = p^.Left^.RBot)) then
              begin
                inc(Completed);
                for i := p^.Left^.R to p^.L do
                    classifyMap[p^.Y, i] := p^.Classify;
                done := True;
              end;
          end;
      end;
    disposeObject(tmpSeg);
  until Completed = 0;
end;

procedure ClassifyMapConvolution(Width, Height: Integer; var classifyMap: TMorphologySegClassifyMap; Classify: TMorphologyClassify;
  Operation_: TBinaryzationOperation; ConvolutionKernel: TMorphologyBinaryzation);
var
  tmp_Binaryzation: TMorphologyBinaryzation;
  i, j: Integer;
begin
  // map Classify to Binaryzation
  tmp_Binaryzation := TMorphologyBinaryzation.Create;
  tmp_Binaryzation.SetSize(Width, Height, False);
  for j := 0 to Height - 1 do
    for i := 0 to Width - 1 do
      if classifyMap[j, i] = Classify then
        begin
          tmp_Binaryzation[i, j] := True;
          classifyMap[j, i] := 0;
        end;
  // Convolution operation
  tmp_Binaryzation.Process(Operation_, ConvolutionKernel);
  // rebuild Classify
  for j := 0 to Height - 1 do
    for i := 0 to Width - 1 do
      if tmp_Binaryzation[i, j] then
          classifyMap[j, i] := Classify;
  // free
  disposeObject(tmp_Binaryzation);
end;

procedure ClassifyMapConvolution(Width, Height: Integer; var classifyMap: TMorphologySegClassifyMap;
  Operations: array of TBinaryzationOperation; ConvolutionKernel: TMorphologyBinaryzation; MaxClassifyCount, MinGranularity: Integer);
type
  TClassifyHash = record
    GranularitySum: Int64;
    Classify: TMorphologyClassify;
  end;

  PClassifyHash = ^TClassifyHash;

  TArrayPClassifyHash = array of PClassifyHash;

  procedure SortGranularity(var arry_: TArrayPClassifyHash);

    function Compare_(Item1, Item2: PClassifyHash): ShortInt;
    begin
      if Item1^.GranularitySum < Item2^.GranularitySum then
          Result := 1
      else if Item1^.GranularitySum > Item2^.GranularitySum then
          Result := -1
      else
          Result := 0;
    end;

    procedure fastSort_(L, R: Integer);
    var
      i, j: TGeoInt;
      p, S: PClassifyHash;
    begin
      repeat
        i := L;
        j := R;
        p := arry_[(L + R) shr 1];
        repeat
          while Compare_(arry_[i], p) < 0 do
              inc(i);
          while Compare_(arry_[j], p) > 0 do
              dec(j);
          if i <= j then
            begin
              if i <> j then
                begin
                  S := arry_[i];
                  arry_[i] := arry_[j];
                  arry_[j] := S;
                end;
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
    if length(arry_) > 1 then
        fastSort_(Low(arry_), High(arry_));
  end;

var
  i, j: Integer;
  tmpHashEng: TUInt32HashPointerList;
  tmpHashData: PClassifyHash;
  tmpHashBuff: TArrayPClassifyHash;
  tmpHashDataPtr: PUInt32HashListPointerStruct;
begin
  // init tmpHashEng eng
  tmpHashEng := TUInt32HashPointerList.CustomCreate($FFFF);
  tmpHashEng.AutoFreeData := False;
  tmpHashEng.AccessOptimization := True;

  for j := 0 to Height - 1 do
    for i := 0 to Width - 1 do
      begin
        // extract classify to tmpHashEng
        if classifyMap[j, i] > 0 then
          begin
            tmpHashData := tmpHashEng[classifyMap[j, i]];
            if tmpHashData = nil then
              begin
                new(tmpHashData);
                tmpHashData^.GranularitySum := 0;
                tmpHashData^.Classify := classifyMap[j, i];
                tmpHashEng.Add(classifyMap[j, i], tmpHashData, False);
              end;
            inc(tmpHashData^.GranularitySum);
          end;
      end;

  // extract tmpHashEng classify to tmpHashBuff
  SetLength(tmpHashBuff, tmpHashEng.Count);
  i := 0;
  tmpHashDataPtr := tmpHashEng.FirstPtr;
  while i < tmpHashEng.Count do
    begin
      tmpHashBuff[i] := tmpHashDataPtr^.Data;
      inc(i);
      tmpHashDataPtr := tmpHashDataPtr^.Next;
    end;

  // sort GranularitySum, from large to little
  SortGranularity(tmpHashBuff);

  // remove noise
  for j := 0 to Height - 1 do
    for i := 0 to Width - 1 do
      begin
        tmpHashData := tmpHashEng[classifyMap[j, i]];
        if (tmpHashData <> nil) and (tmpHashData^.GranularitySum <= MinGranularity) then
            classifyMap[j, i] := 0;
      end;

  // remove little granularity
  if MaxClassifyCount - 1 > length(tmpHashBuff) then
    begin
      tmpHashEng.Clear;
      for i := MaxClassifyCount - 1 to High(tmpHashBuff) do
          tmpHashEng.Add(tmpHashBuff[i]^.Classify, tmpHashBuff[i], False);

      for j := 0 to Height - 1 do
        for i := 0 to Width - 1 do
          if tmpHashEng.Exists(classifyMap[j, i]) then
              classifyMap[j, i] := 0;
    end;

  // morphology binaryzation convolution
  for i := Low(tmpHashBuff) to umlMin(MaxClassifyCount - 1, High(tmpHashBuff)) do
    if tmpHashBuff[i]^.GranularitySum > MinGranularity then
      begin
        for j := Low(Operations) to High(Operations) do
            ClassifyMapConvolution(Width, Height, classifyMap, tmpHashBuff[i]^.Classify, Operations[j], ConvolutionKernel);
      end
    else
        break;

  // free temp
  for i := Low(tmpHashBuff) to High(tmpHashBuff) do
      Dispose(tmpHashBuff[i]);
  SetLength(tmpHashBuff, 0);
  disposeObject(tmpHashEng);
end;

procedure TestRasterSavePerformance(inputfile: SystemString);
var
  m64, tmp: TMemoryStream64;
  f: TRasterSaveFormat;
  raster: TRaster;
  tk: TTimeTick;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(inputfile);

  for f := Low(TRasterSaveFormat) to high(TRasterSaveFormat) do
    begin
      tmp := TMemoryStream64.CustomCreate(64 * 1024 * 1024);
      m64.Position := 0;
      raster := NewRasterFromStream(m64);
      raster.FitScale(1920, 1920);
      tk := GetTimeTick;
      raster.SaveToStream(tmp, f);
      DoStatus('%s save %dms', [GetEnumName(TypeInfo(TRasterSaveFormat), Ord(f)), GetTimeTick - tk]);

      raster.Reset;
      tmp.Position := 0;
      tk := GetTimeTick;
      raster.LoadFromStream(tmp);
      DoStatus('%s load %dms', [GetEnumName(TypeInfo(TRasterSaveFormat), Ord(f)), GetTimeTick - tk]);
      disposeObject(raster);
      disposeObject(tmp);

      DoStatus('');
    end;

  disposeObject(m64);
end;
