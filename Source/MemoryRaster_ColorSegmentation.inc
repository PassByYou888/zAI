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

constructor TSegPool.Create;
begin
  inherited Create;
  FBoundsCached := False;
  FBoundsCache := NullRectV2;
  Owner := nil;
end;

procedure TSegPool.AddSeg(const buff: array of PSeg);
var
  f: array of Boolean;
  i, j: Integer;
  p: PSeg;
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

procedure TSegPool.SortY;

  function Sort_Compare(const Item1, Item2: PSeg): Integer;
  begin
    if Item1^.Y < Item2^.Y then
        Result := -1
    else if Item1^.Y > Item2^.Y then
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

function TSegPool.Classify: TSegClassify;
begin
  if Count > 0 then
      Result := First^.Classify
  else
      Result := 0;
end;

function TSegPool.BoundsRectV2(cache: Boolean): TRectV2;
var
  i: Integer;
  p: PSeg;
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

      FBoundsCache := Result;
      FBoundsCached := True;
    end
  else
      Result := NullRectV2;
end;

function TSegPool.BoundsRectV2: TRectV2;
begin
  Result := BoundsRectV2(True);
end;

function TSegPool.BoundsRect: TRect;
begin
  Result := Rect2Rect(BoundsRectV2);
end;

function TSegPool.Left: Integer;
begin
  Result := BoundsRect.Left;
end;

function TSegPool.Top: Integer;
begin
  Result := BoundsRect.Top;
end;

function TSegPool.Width: Integer;
begin
  Result := Round(RectWidth(BoundsRectV2)) + 1;
end;

function TSegPool.Height: Integer;
begin
  Result := Round(RectHeight(BoundsRectV2)) + 1;
end;

function TSegPool.PixelArea: Integer;
var
  i: Integer;
  p: PSeg;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      inc(Result, p^.R - p^.L);
    end;
end;

function TSegPool.Area: Integer;
begin
  Result := Round(RectArea(BoundsRectV2));
end;

procedure TSegPool.FillTo(dest: TMemoryRaster; DataColor: TRColor);
var
  i, j: Integer;
  p: PSeg;
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

function TSegPool.BuildDatamap(backColor, DataColor: TRColor): TMemoryRaster;
begin
  Result := NewRaster();
  Result.SetSize(Owner.Width, Owner.Height, backColor);
  FillTo(Result, DataColor);
end;

function TSegPool.BuildClipDatamap(backColor, DataColor: TRColor): TMemoryRaster;
var
  R: TRect;
  i, j: Integer;
  p: PSeg;
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

function TSegPool.BuildClipMap(Source: TMemoryRaster; backColor: TRColor): TMemoryRaster;
var
  R: TRect;
  i, j: Integer;
  p: PSeg;
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

function TColorSegmentation.NewSeg(X, Y: Integer; Classify: TSegClassify): PSeg;
begin
  new(Result);
  Result^.Y := Y;
  Result^.L := X;
  Result^.R := X;
  Result^.LTop := nil;
  Result^.RTop := nil;
  Result^.LBot := nil;
  Result^.RBot := nil;
  Result^.GroupID := -1;
  Result^.Classify := Classify;
  FSource.Add(Result);
  FSegMap[Y, X] := Result;
end;

function TColorSegmentation.FindSegPool(p: PSeg): TSegPool;
var
  i, j: Integer;
  sp: TSegPool;
  p2: PSeg;
begin
  Result := nil;

  if p <> nil then
    for j := 0 to FSegPoolGroup.Count - 1 do
      begin
        sp := FSegPoolGroup[j];

        for i := 0 to sp.Count - 1 do
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

function TColorSegmentation.GetOrCreateSegPool(p: PSeg): TSegPool;
begin
  Result := FindSegPool(p);
  if Result = nil then
    begin
      Result := TSegPool.Create;
      Result.Owner := Self;
      FSegPoolGroup.Add(Result);
    end;
end;

procedure TColorSegmentation.AddSegToGroup(p: PSeg);
var
  nPool: TSegPool;
begin
  if p = nil then
      exit;
  nPool := GetOrCreateSegPool(p);
  nPool.AddSeg([p, p^.LTop, p^.RTop, p^.LBot, p^.RBot]);
end;

function TColorSegmentation.GetSegPool(X, Y: Integer): TSegPool;
var
  p: PSeg;
begin
  Result := nil;
  p := FSegMap[Y, X];
  if p = nil then
      exit;
  Result := FSegPoolGroup[p^.GroupID];
end;

function TColorSegmentation.GetItems(index: Integer): TSegPool;
begin
  Result := FSegPoolGroup[index];
end;

constructor TColorSegmentation.CustomCreate(Raster_: TMemoryRaster; BoundsRect_: TRect);
var
  i, j: Integer;
begin
  inherited Create;
  FRaster := Raster_;
  FRasterBoundsRect := ForwardRect(BoundsRect_);

  FWidth := FRaster.FWidth;
  FHeight := FRaster.FHeight;
  SetLength(FSegMap, FHeight, FWidth);
  FSource := TSegPool_Decl.Create;
  FSegPoolGroup := TSegPoolGroup.Create;

  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
        FSegMap[j, i] := nil;

  FOnSegColor := nil;
end;

constructor TColorSegmentation.Create(Raster_: TMemoryRaster);
begin
  CustomCreate(Raster_, Raster_.BoundsRect);
end;

destructor TColorSegmentation.Destroy;
var
  i: Integer;
begin
  SetLength(FSegMap, 0, 0);
  for i := 0 to FSource.Count - 1 do
      dispose(FSource[i]);
  DisposeObject(FSource);
  for i := 0 to FSegPoolGroup.Count - 1 do
      DisposeObject(FSegPoolGroup[i]);
  DisposeObject(FSegPoolGroup);

  inherited Destroy;
end;

function TColorSegmentation.DoSegColor(Color: TRColor): TSegClassify;
begin
  Result := 0;
  if Assigned(FOnSegColor) then
      FOnSegColor(Color, Result)
  else if Color = $FFFFFFFF then
      Result := 0
  else
      Result := RColor2Gray(Color);
end;

procedure TColorSegmentation.BuildSegmentation;
var
  Classify, L_id: TSegClassify;
  i, j: Integer;
  p: PSeg;
  L, R: PSeg;
begin
  // reset
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
        FSegMap[j, i] := nil;

  for i := 0 to FSource.Count - 1 do
      dispose(FSource[i]);
  FSource.Clear;

  for i := 0 to FSegPoolGroup.Count - 1 do
      DisposeObject(FSegPoolGroup[i]);
  FSegPoolGroup.Clear;

  // fill liner seg
  for j := FRasterBoundsRect.Top to FRasterBoundsRect.Bottom - 1 do
    begin
      L_id := 0;
      for i := FRasterBoundsRect.Left to FRasterBoundsRect.Right - 1 do
        begin
          Classify := DoSegColor(FRaster[i, j]);
          if Classify > 0 then
            begin
              if (i > 0) and (L_id = Classify) then
                begin
                  p := FSegMap[j, i - 1];
                  p^.R := i;
                  FSegMap[j, i] := p;
                end
              else
                  p := NewSeg(i, j, Classify);
            end;
          L_id := Classify;
        end;
    end;

  // fill horz link
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

  // extract seg group
  for j := 0 to FSource.Count - 1 do
      AddSegToGroup(FSource[j]);

  UpdateSegMap;
end;

procedure TColorSegmentation.UpdateSegMap;
var
  i, j: Integer;
  p: PSeg;
  sp, found_sp: TSegPool;
  re_sp: Boolean;
begin
  // combine group for far distance
  j := 0;
  while j < FSegPoolGroup.Count do
    begin
      sp := FSegPoolGroup[j];
      repeat
        re_sp := False;
        i := 0;
        while i < sp.Count do
          begin
            p := sp[i];
            found_sp := FindSegPool(p);
            if found_sp <> sp then
              begin
                AddSegToGroup(p);
                sp.Delete(i);
                re_sp := True;
              end
            else
                inc(i);
          end;
      until not re_sp;
      if sp.Count = 0 then
        begin
          FSegPoolGroup.Delete(j);
          DisposeObject(sp);
        end
      else
          inc(j);
    end;

  // reset group id
  for j := 0 to FSegPoolGroup.Count - 1 do
    begin
      sp := FSegPoolGroup[j];
      sp.SortY;
      sp.FBoundsCached := False;
      for i := 0 to sp.Count - 1 do
          sp[i]^.GroupID := j;
    end;
end;

procedure TColorSegmentation.MergeOverlapSegmentation;
  function MergeToPool(pool: TSegPool): Boolean;
  var
    r1, r2: TRectV2;
    i, j: Integer;
    rect_overlap: Boolean;
    p: PSeg;
    sp: TSegPool;
  begin
    Result := False;
    r1 := pool.BoundsRectV2(False);
    i := 0;
    for i := 0 to FSegPoolGroup.Count - 1 do
      begin
        if (FSegPoolGroup[i] <> pool) and (FSegPoolGroup[i].Classify = pool.Classify) then
          begin
            sp := FSegPoolGroup[i];
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
                DisposeObject(sp);
                FSegPoolGroup.Delete(i);
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
  while i < FSegPoolGroup.Count do
    if MergeToPool(FSegPoolGroup[i]) then
        i := 0
    else
        inc(i);

  UpdateSegMap;
end;

function TColorSegmentation.RemoveNoise(PixelNoiseThreshold: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  i := 0;
  while i < FSegPoolGroup.Count do
    if FSegPoolGroup[i].PixelArea < PixelNoiseThreshold then
      begin
        DisposeObject(FSegPoolGroup[i]);
        FSegPoolGroup.Delete(i);
        Result := True;
      end
    else
        inc(i);

  if Result then
      UpdateSegMap;
end;

function TColorSegmentation.Count: Integer;
begin
  Result := FSegPoolGroup.Count;
end;

procedure test_ColorSegmentation(inputfile, outputfile: SystemString);
var
  R: TMemoryRaster;
  colors: array of TRColor;
  i, j: Integer;
  sp: TSegPool;
begin
  R := NewRasterFromFile(inputfile);

  with TColorSegmentation.Create(R) do
    begin
      BuildSegmentation;
      R.Clear(RColor(0, 0, 0));
      SetLength(colors, FSegPoolGroup.Count);
      for i := 0 to FSegPoolGroup.Count - 1 do
          colors[i] := RandomRColor;

      for j := 0 to Count - 1 do
        begin
          sp := Items[j];
          sp.FillTo(R, colors[j]);
        end;
      Free;
    end;
  R.SaveToFile(outputfile);
  DisposeObject(R);
end;

constructor TRCLines.Create(raster: TMemoryRaster; DetectorColor: TRColor; Threshold: TGeoFloat; MinLineLength: Integer);
var
  LMap: array of array of Boolean;
  X, Y: Integer;
  isNear: Boolean;
  B, E: Integer;
begin
  inherited Create;
  SetLength(LMap, raster.Height, raster.Width);
  for Y := 0 to raster.Height - 1 do
    for X := 0 to raster.Width - 1 do
        LMap[Y, X] := RColorDistance(raster[X, Y], DetectorColor) <= Threshold;

  // build x lines
  for Y := 0 to raster.Height - 1 do
    begin
      isNear := False;
      for X := 0 to raster.Width - 1 do
        begin
          if (LMap[Y, X]) and (not isNear) then
            begin
              B := X;
              isNear := True;
            end
          else if (not LMap[Y, X]) and (isNear) then
            begin
              E := X - 1;
              isNear := False;
              if E - B >= MinLineLength then
                  AddRCLine(B, Y, E, Y, lsRow);
            end;
        end;
    end;

  // build y lines
  for X := 0 to raster.Width - 1 do
    begin
      isNear := False;
      for Y := 0 to raster.Height - 1 do
        begin
          if (LMap[Y, X]) and (not isNear) then
            begin
              B := Y;
              isNear := True;
            end
          else if (not LMap[Y, X]) and (isNear) then
            begin
              E := Y - 1;
              isNear := False;
              if E - B >= MinLineLength then
                  AddRCLine(X, B, X, E, lsCol);
            end;
        end;
    end;
  SetLength(LMap, 0, 0);
end;

destructor TRCLines.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TRCLines.AddRCLine(Bx, By, Ex, Ey: Integer; Style: TRCLineStyle);
var
  p: PRCLine;
begin
  new(p);
  p^.Bp := Point(Bx, By);
  p^.Ep := Point(Ex, Ey);
  p^.Style := Style;
  Add(p);
end;

function TRCLines.ProcessFormulaBox(var box: TRectV2): Boolean;
  function f(Pt: TPoint; X, Y: Boolean; Style: TRCLineStyle; var index: Integer): PRCLine;
  var
    i: Integer;
    p: PRCLine;
  begin
    for i := index to Count - 1 do
      begin
        p := Items[i];
        if ((not X) or (p^.Bp.X = Pt.X)) and ((not Y) or (p^.Bp.Y = Pt.Y)) and (p^.Style = Style) then
          begin
            index := i;
            Result := p;
            exit;
          end;
      end;
    Result := nil;
  end;

var
  i: Integer;
  p: PRCLine;
  Left, Right, bot: PRCLine;
begin
  while Count > 0 do
    begin
      p := Items[0];
      if p^.Style = lsRow then
        begin
          i := 1;
          bot := f(p^.Bp, True, False, lsRow, i);
          if (bot <> nil) and (bot^.Ep.X = p^.Ep.X) then
            begin
              i := i + 1;
              Left := f(p^.Bp, True, True, lsCol, i);
              if (Left <> nil) and (Left^.Ep.Y = bot^.Bp.Y) then
                begin
                  i := i + 1;
                  Right := f(p^.Ep, True, True, lsCol, i);
                  if (Right <> nil) and (Right^.Ep.X = bot^.Ep.X) and (Right^.Ep.Y = bot^.Ep.Y) then
                    begin
                      box[0, 0] := p^.Bp.X;
                      box[0, 1] := p^.Bp.Y;
                      box[1, 0] := bot^.Ep.X;
                      box[1, 1] := bot^.Ep.Y;
                      Remove(p, bot, Left, Right);
                      Result := True;
                      exit;
                    end;
                end;
            end;
        end;
      Delete(0);
    end;
  Result := False;
end;

procedure TRCLines.Remove(p1, p2, p3, p4: PRCLine);
var
  i: Integer;
  p: PRCLine;
begin
  i := 0;
  while i < Count do
    begin
      p := Items[i];
      if (p = p1) or (p = p2) or (p = p3) or (p = p4) then
          Delete(i)
      else
          inc(i);
    end;
end;

procedure TRCLines.Remove(p: PRCLine);
begin
  dispose(p);
  inherited Remove(p);
end;

procedure TRCLines.Delete(index: Integer);
begin
  if (index >= 0) and (index < Count) then
    begin
      dispose(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TRCLines.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      dispose(Items[i]);
  inherited Clear;
end;
