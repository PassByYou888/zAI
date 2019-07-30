{$IFDEF LARGE_RASTER_FONT}
{$RESOURCE MemoryRaster_LargeFont.RES}
{$ELSE LARGE_RASTER_FONT}
{$RESOURCE MemoryRaster_DefaultFont.RES}
{$ENDIF LARGE_RASTER_FONT}


procedure InitSystemFont_Thread_Run(Sender: TComputeThread);
var
  rs: TCoreClassResourceStream;
  fn: TFontRaster;
begin
  rs := TCoreClassResourceStream.Create(HInstance, 'MemoryRasterFont', RT_RCDATA);
  rs.Position := 0;

  fn := TFontRaster.Create;
  fn.LoadFromStream(rs);
  DisposeObject(rs);

  SystemFont := fn;
end;

procedure Init_DefaultFont;
begin
  SystemFont := nil;
  TComputeThread.RunC(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}InitSystemFont_Thread_Run);
end;

procedure Free_DefaultFont;
begin
  Wait_SystemFont_Init;
  DisposeObject(SystemFont);
  SystemFont := nil;
end;

procedure Wait_SystemFont_Init;
begin
  while SystemFont = nil do
      CheckThreadSynchronize(10);
end;

function fr_ComputeNearest(const Sender: PVertexMap; const Sampler: TMemoryRaster; const x, y: TGeoFloat): TRColor;
var
  FR: TFontRaster;
  tu0, tv0: Integer;
begin
  FR := TFontRaster.PFontDrawState(Sender^.UserData)^.Owner;
  tu0 := ClampInt(Trunc(TexCoordWrapRepeat(x) * (Sampler.Width0)), 0, Sampler.Width0i);
  tv0 := ClampInt(Trunc(TexCoordWrapRepeat(y) * (Sampler.Height0)), 0, Sampler.Height0i);
  Result := RColor(0, 0, 0, FR.FBitRaster^[tu0 + tv0 * Sampler.Width]);
end;

function fr_ComputeLinear(const Sender: PVertexMap; const Sampler: TMemoryRaster; const x, y: TGeoFloat): TRColor;
var
  FR: TFontRaster;
  fx, fy: TGeoFloat;
  i_x, i_y: Integer;
  i_x2, i_y2: Integer;
  delta_x, delta_y: TGeoFloat;
  k1, k2, k3, K4: TGeoFloat;
  c1, c2, c3, c4: Byte;
  g: TGeoFloat;
begin
  FR := TFontRaster.PFontDrawState(Sender^.UserData)^.Owner;

  fx := TexCoordWrapRepeat(x) * (Sampler.Width0);
  fy := TexCoordWrapRepeat(y) * (Sampler.Height0);

  i_x := ClampInt(Trunc(fx), 0, Sampler.Width0i);
  i_y := ClampInt(Trunc(fy), 0, Sampler.Height0i);

  i_x2 := i_x + 1;
  i_y2 := i_y + 1;
  if (i_x2 > Sampler.Width0) or (i_y2 > Sampler.Height0) then
    begin
      Result := FR.FBitRaster^[i_x + i_y * Sampler.Width];
      exit;
    end;

  delta_x := Frac(fx);
  delta_y := Frac(fy);

  k1 := (1 - delta_x) * (1 - delta_y);
  k2 := delta_x * (1 - delta_y);
  k3 := delta_x * delta_y;
  K4 := (1 - delta_x) * delta_y;

  c1 := FR.FBitRaster^[i_x + i_y * Sampler.Width];
  c2 := FR.FBitRaster^[i_x2 + i_y * Sampler.Width];
  c3 := FR.FBitRaster^[i_x2 + i_y2 * Sampler.Width];
  c4 := FR.FBitRaster^[i_x + i_y2 * Sampler.Width];

  g := ((c1 / $FF) * k1) + ((c2 / $FF) * k2) + ((c3 / $FF) * k3) + ((c4 / $FF) * K4);

  if g < 0 then
      g := 0
  else if g > 1.0 then
      g := 1.0;

  Result := RColor(0, 0, 0, Round(g * $FF));
end;

procedure fr_ComputeBlend(const Sender: PVertexMap; const f, M: TRColor; var b: TRColor);
var
  c: TRColorEntry;
begin
  c.BGRA := TFontRaster.PFontDrawState(Sender^.UserData)^.DestColor;
  c.a := TRColorEntry(f).a;
  Vertex_ComputeBlend(Sender, c.BGRA, M, b);
end;

constructor TFontRaster.Create;
var
  i: Integer;
begin
  inherited Create;
  FOnlyInstance := True;
  FFontTable := GetMemory(C_MAXWORD * SizeOf(TFontCharDefine));
  SetLength(FFragRaster, C_MAXWORD);
  for i := 0 to C_MAXWORD - 1 do
    begin
      FFontTable^[i] := C_WordDefine;
      FFragRaster[i] := nil;
    end;
  FBitRaster := nil;
  FFontSize := 0;
  FActivtedWord := 0;
  FWidth := 0;
  FHeight := 0;
end;

constructor TFontRaster.Create(ShareFont: TFontRaster);
begin
  inherited Create;
  FOnlyInstance := False;
  FFontTable := ShareFont.FFontTable;
  SetLength(FFragRaster, 0);
  FBitRaster := ShareFont.FBitRaster;
  FFontSize := ShareFont.FFontSize;
  FActivtedWord := ShareFont.FActivtedWord;
  FWidth := ShareFont.FWidth;
  FHeight := ShareFont.FHeight;
end;

destructor TFontRaster.Destroy;
begin
  Clear;
  if FOnlyInstance then
    begin
      FreeMemory(FFontTable);
      SetLength(FFragRaster, 0);
    end;
  inherited Destroy;
end;

procedure TFontRaster.Add(c: TFontRasterChar; raster: TMemoryRaster);
begin
  Remove(c);
  FFragRaster[Word(c)] := raster;
end;

procedure TFontRaster.Remove(c: TFontRasterChar);
var
  i: Word;
begin
  i := Word(c);
  if FOnlyInstance then
      FFontTable^[i] := C_WordDefine;
  if FFragRaster[i] <> nil then
    begin
      DisposeObject(FFragRaster[i]);
      FFragRaster[i] := nil;
    end;
end;

procedure TFontRaster.Clear;
var
  i: Integer;
begin
  if FOnlyInstance then
    begin
      for i := 0 to C_MAXWORD - 1 do
        begin
          FFontTable^[i] := C_WordDefine;
          if FFragRaster[i] <> nil then
            begin
              DisposeObject(FFragRaster[i]);
              FFragRaster[i] := nil;
            end;
        end;

      FreeMemory(FBitRaster);
      FBitRaster := nil;

      FActivtedWord := 0;
      FWidth := 0;
      FHeight := 0;
    end;
end;

procedure TFontRaster.Build(fontSiz: Integer);
var
  rHash: THashList;
  HashMapTable: array [0 .. C_MAXWORD - 1] of PFontCharDefine;
  RP: TRectPacking;
  M5: TFontRasterString;
  HP: Pointer;
  i, x, y: Integer;
  Rect_p: PRectPackData;
  fontchar_p: PFontCharDefine;
  raster: TMemoryRaster;
  r: TRect;
  FR, Fg, fb, FA: TGeoFloat;
  gb: Byte;

begin
  if not FOnlyInstance then
      exit;

  DoStatus('build font Hash...');
  rHash := THashList.CustomCreate(8192);
  rHash.AutoFreeData := False;

  RP := TRectPacking.Create;
  for i := 0 to C_MAXWORD - 1 do
    begin
      fontchar_p := @(FFontTable^[i]);
      fontchar_p^ := C_WordDefine;
      HashMapTable[i] := fontchar_p;
      raster := FFragRaster[i];
      if raster <> nil then
        begin
          M5 := umlMD5String(@raster.Bits^[0], raster.Width * raster.Height * SizeOf(TRColor));
          HP := rHash[M5];
          if HP = nil then
            begin
              rHash.Add(M5, fontchar_p, True);
              RP.Add(fontchar_p, raster, raster.BoundsRectV2);
            end
          else
              HashMapTable[i] := PFontCharDefine(HP);
        end;
    end;
  DisposeObject(rHash);

  if RP.Count > 0 then
    begin
      DoStatus('build font space...');
      RP.Margins := 2;
      RP.Build(C_MAXWORD * 1024, C_MAXWORD * 1024);

      FActivtedWord := 0;
      FWidth := Round(RP.MaxWidth);
      FHeight := Round(RP.MaxHeight);

      FBitRaster := GetMemory(FWidth * FHeight);
      FillPtrByte(FBitRaster, FWidth * FHeight, 0);

      DoStatus('compress font raster...');
      for i := 0 to RP.Count - 1 do
        begin
          Rect_p := RP[i];
          if Rect_p^.error then
            begin
              DoStatus('wordRaster Build Error');
              Continue;
            end;
          fontchar_p := PFontCharDefine(Rect_p^.Data1);
          raster := TMemoryRaster(Rect_p^.Data2);

          r := MakeRect(Rect_p^.Rect);

          // write word
          fontchar_p^.Activted := True;
          fontchar_p^.x := r.Left;
          fontchar_p^.y := r.Top;
          fontchar_p^.w := r.Right - r.Left;
          fontchar_p^.h := r.Bottom - r.Top;

          // write raster
          for y := 0 to raster.Height - 1 do
            for x := 0 to raster.Width - 1 do
                FBitRaster^[(y + r.Top) * FWidth + (x + r.Left)] := RColor2Gray(raster[x, y]);

          inc(FActivtedWord);
        end;

      DoStatus('merge font space...');
      for i := 0 to C_MAXWORD - 1 do
        begin
          if HashMapTable[i] <> @(FFontTable^[i]) then
              FFontTable^[i] := HashMapTable[i]^;

          if FFragRaster[i] <> nil then
            begin
              DisposeObject(FFragRaster[i]);
              FFragRaster[i] := nil;
            end;
        end;
    end;

  DisposeObject(RP);

  FFontSize := fontSiz;

  DoStatus('build font done.');
end;

procedure TFontRaster.LoadFromStream(stream: TCoreClassStream);
var
  m64: TMemoryStream64;
begin
  if not FOnlyInstance then
      exit;

  Clear;
  m64 := TMemoryStream64.Create;
  DecompressStream(stream, m64);
  m64.Position := 0;

  m64.read(FFontSize, 4);
  m64.read(FActivtedWord, 4);
  m64.read(FWidth, 4);
  m64.read(FHeight, 4);
  FBitRaster := GetMemory(FWidth * FHeight);
  m64.read(FBitRaster^[0], FWidth * FHeight);
  m64.read(FFontTable^[0], C_MAXWORD * SizeOf(TFontCharDefine));

  DisposeObject(m64);
end;

procedure TFontRaster.SaveToStream(stream: TCoreClassStream);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.write(FFontSize, 4);
  m64.write(FActivtedWord, 4);
  m64.write(FWidth, 4);
  m64.write(FHeight, 4);
  m64.write(FBitRaster^[0], FWidth * FHeight);
  m64.write(FFontTable^[0], C_MAXWORD * SizeOf(TFontCharDefine));

  MaxCompressStream(m64, stream);
  DisposeObject(m64);

  stream.Position := 0;
end;

procedure TFontRaster.LoadFromFile(filename: TPascalString);
var
  stream: TCoreClassStream;
begin
  stream := TCoreClassFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
      LoadFromStream(stream);
  except
  end;
  DisposeObject(stream);
end;

procedure TFontRaster.SaveToFile(filename: TPascalString);
var
  stream: TCoreClassStream;
begin
  stream := TCoreClassFileStream.Create(filename, fmCreate);
  try
      SaveToStream(stream);
  except
  end;
  DisposeObject(stream);
end;

procedure TFontRaster.ExportRaster(stream: TCoreClassStream; partitionLine: Boolean);
var
  raster: TMemoryRaster;
  y, x: Integer;
  FD: PFontCharDefine;
  r: TRect;
begin
  raster := TMemoryRaster.Create;
  raster.SetSize(FWidth, FHeight, RColor(0, 0, 0));
  for y := 0 to FHeight - 1 do
    for x := 0 to FWidth - 1 do
        raster.PixelGray[x, y] := FBitRaster^[y * FWidth + x];

  if partitionLine then
    for x := 0 to C_MAXWORD - 1 do
      begin
        FD := @(FFontTable^[x]);
        r := Rect(FD^.x, FD^.y, FD^.x + FD^.w, FD^.y + FD^.h);
        raster.DrawRect(r, RColor($FF, 0, 0, 200));
      end;

  raster.SaveToStream(stream);
  DisposeObject(raster);
end;

function TFontRaster.CharSize(const c: TFontRasterChar): TPoint;
begin
  with FFontTable^[Word(c)] do
      Result := Point(w, h);
end;

function TFontRaster.TextSize(const s: TFontRasterString; charVec2List: TVec2List): TVec2;
var
  i: Integer;
  c: TFontRasterChar;
  FD: PFontCharDefine;
  RP: TVec2;
  MH: Byte;
begin
  Result := vec2(0, 0);
  RP := vec2(0, 0);
  MH := 0;
  for i := 1 to s.Len do
    begin
      c := s[i];
      FD := @(FFontTable^[Word(c)]);
      if FD^.h > MH then
          MH := FD^.h;

      if c = #10 then
        begin
          RP[0] := 0;
          RP[1] := RP[1] + MH + 1;
          MH := 0;
          Continue;
        end;

      if c = #9 then
        begin
          RP[0] := RP[0] + (FFontTable^[32].w + 1) * 2;
          Continue;
        end;

      if c = #13 then
          Continue;

      if not FD^.Activted then
          Continue;

      if charVec2List <> nil then
          charVec2List.Add(RP[0] + (FD^.w + 1) * 0.5, RP[1] + (FD^.h + 1) * 0.5);
      RP[0] := RP[0] + FD^.w;
      Result[0] := Max(Result[0], RP[0]);
    end;
  Result[1] := RP[1] + MH + 1;
end;

function TFontRaster.TextSize(const s: TFontRasterString): TVec2;
begin
  Result := TextSize(s, nil);
end;

function TFontRaster.TextWidth(const s: TFontRasterString): Word;
var
  v: TVec2;
begin
  v := TextSize(s);
  Result := Round(v[0]);
end;

function TFontRaster.TextHeight(const s: TFontRasterString): Word;
var
  v: TVec2;
begin
  v := TextSize(s);
  Result := Round(v[1]);
end;

function TFontRaster.Draw(Text: TFontRasterString; Dst: TMemoryRaster; dstVec: TVec2; dstColor: TRColor;
  const bilinear_sampling: Boolean; const alpha: TGeoFloat; const axis: TVec2; const angle, Scale: TGeoFloat): TVec2;
var
  bak_userData: Pointer;
  DD: TFontDrawState;
  TempSampler: TMemoryRaster;
  VM: TVertexMap;
  i: Integer;
  c: TFontRasterChar;
  FD: PFontCharDefine;
  cv: TVec2;
  s: TRectV2;
  MH: Byte;
  vSiz: TVec2;
  wp: TDeflectionPolygon;
  vl: TVec2List;
  WI: Integer;
begin
  // init triangle raster
  DD.Owner := Self;
  DD.DestColor := dstColor;
  TempSampler := TMemoryRaster.Create;
  TempSampler.SetWorkMemory(nil, FWidth, FHeight);

  VM := Dst.Vertex;
  bak_userData := VM.UserData;
  VM.ComputeNearest := {$IFDEF FPC}@{$ENDIF FPC}fr_ComputeNearest;
  VM.ComputeLinear := {$IFDEF FPC}@{$ENDIF FPC}fr_ComputeLinear;
  VM.ComputeBlend := {$IFDEF FPC}@{$ENDIF FPC}fr_ComputeBlend;
  VM.UserData := @DD;

  // compute rotate
  vl := TVec2List.Create;
  vSiz := TextSize(Text, vl);
  wp := TDeflectionPolygon.Create;
  wp.Position := Vec2Mul(vSiz, axis);
  for WI := 0 to vl.Count - 1 do
      wp.AddPoint(vl[WI]^);
  DisposeObject(vl);
  wp.Position := Vec2Add(dstVec, Vec2Mul(vSiz, axis));
  wp.angle := angle;
  wp.Scale := Scale;
  WI := 0;

  VM.BeginUpdate;
  try
    for i := 1 to Text.Len do
      begin
        c := Text[i];
        FD := @(FFontTable^[Word(c)]);

        if not FD^.Activted then
            Continue;
        if CharIn(c, [#10, #9, #13]) then
            Continue;

        // projection
        s := RectV2(FD^.x, FD^.y, FD^.x + FD^.w + 1, FD^.y + FD^.h + 1);
        cv := wp.Points[WI];
        VM.DrawRect(
          TV2Rect4.Init(s, 0),
          TV2Rect4.Init(cv, (FD^.w + 1) * Scale, (FD^.h + 1) * Scale, angle),
          TempSampler,
          bilinear_sampling,
          alpha);

        inc(WI);
      end;
  finally
    VM.EndUpdate;

    DisposeObject(wp);
    DisposeObject(TempSampler);

    // restore vertex parameter
    VM.ComputeBlend := {$IFDEF FPC}@{$ENDIF FPC}Vertex_ComputeBlend;
    VM.ComputeNearest := {$IFDEF FPC}@{$ENDIF FPC}Vertex_ComputeNearest;
    VM.ComputeLinear := {$IFDEF FPC}@{$ENDIF FPC}Vertex_ComputeLinear;
    VM.UserData := bak_userData;
  end;
  Result := vSiz;
end;

procedure TFontRaster.Draw(Text: TFontRasterString; Dst: TMemoryRaster; dstVec: TVec2; dstColor: TRColor);
begin
  Draw(Text, Dst, dstVec, dstColor, True, 1.0, vec2(0.5, 0.5), 0, 1);
end;
