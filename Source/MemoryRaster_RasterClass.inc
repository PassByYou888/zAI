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
procedure MakeMergeTables;
var
  i, j: Integer;
const
  OneByteth: Double = 1.0 / $FF;
begin
  for j := 0 to $FF do
    for i := 0 to $FF do
      begin
        DivTable[i, j] := ClampByte(Round(i * j * OneByteth));
        if i > 0 then
            RcTable[i, j] := ClampByte(Round(j * $FF / i))
        else
            RcTable[i, j] := 0;
      end;
end;

function TYIQ.GetRGB: TRColor;
var
  nY, nI, nQ, R, G, B: TGeoFloat;
begin
  nY := Y;
  nI := i * 1.191 - 0.595;
  nQ := Q * 1.045 - 0.523;
  R := nY + 0.956 * nI + 0.621 * nQ;
  G := nY - 0.272 * nI - 0.647 * nQ;
  B := nY - 1.107 * nI + 1.706 * nQ;
  Result := RColorF(R, G, B);
end;

procedure TYIQ.SetRGB(const Value: TRColor);
var
  R, G, B: TGeoFloat;
begin
  RColor2F(Value, R, G, B);
  Y := 0.299 * R + 0.587 * G + 0.114 * B;
  i := 0.596 * R - 0.274 * G - 0.321 * B;
  i := (i + 0.595) / 1.191;
  Q := 0.211 * R - 0.523 * G + 0.311 * B;
  Q := (Q + 0.523) / 1.045;
end;

function TYIQ.GetRGBA(A: Byte): TRColor;
begin
  Result := GetRGB();
  TRColorEntry(Result).A := A;
end;

procedure TYIQ.SetRGBA(A: Byte; const Value: TRColor);
begin
  SetRGB(Value);
end;

function THSI.GetRGB: TRColor;
var
  nH, nS, nI, R, G, B: TGeoFloat;
begin
  nH := H * 363 - 1;
  nS := S;
  nI := i;
  R := 0;
  G := 0;
  B := 0;
  if nH = 0 then
    begin
      R := nI + 2 * nI * nS;
      G := nI - nI * nS;
      B := nI - nI * nS;
    end
  else if (0 < nH) and (nH < 120) then
    begin
      R := nI + nI * nS * Cos(DegToRad(nH)) / Cos(DegToRad(60 - nH));
      G := nI + nI * nS * (1 - Cos(DegToRad(nH)) / Cos(DegToRad(60 - nH)));
      B := nI - nI * nS;
    end
  else if (nH = 120) then
    begin
      R := nI - nI * nS;
      G := nI + 2 * nI * nS;
      B := nI - nI * nS;
    end
  else if (120 < nH) and (nH < 240) then
    begin
      R := nI - nI * nS;
      G := nI + nI * nS * Cos(DegToRad(nH - 120)) / Cos(DegToRad(180 - nH));
      B := nI + nI * nS * (1 - Cos(DegToRad(nH - 120)) / Cos(DegToRad(180 - nH)));
    end
  else if nH = 240 then
    begin
      R := nI - nI * nS;
      G := nI - nI * nS;
      B := nI + 2 * nI * nS;
    end
  else if (240 < nH) and (nH < 360) then
    begin
      R := nI + nI * nS * (1 - Cos(DegToRad(nH - 240)) / Cos(DegToRad(300 - nH)));
      G := nI - nI * nS;
      B := nI + nI * nS * Cos(DegToRad(nH - 240)) / Cos(DegToRad(300 - nH));
    end
  else if nH > 360 then
    begin
      R := nI;
      G := nI;
      B := nI;
    end;
  Result := RColorF(R, G, B);
end;

procedure THSI.SetRGB(const Value: TRColor);
var
  R, G, B: TGeoFloat;
begin
  RColor2F(Value, R, G, B);
  i := (R + G + B) / 3.0;
  if i > 0 then
      S := 1 - umlMin(umlMin(R, G), B) / i
  else
      S := 0;
  if S <> 0 then
    begin
      R := Round(R * 255);
      G := Round(G * 255);
      B := Round(B * 255);
      if G >= B then
          H := RadToDeg(ArcCos((R - G * 0.5 - B * 0.5) / Sqrt(Sqr(R) + Sqr(G) + Sqr(B) - R * G - R * B - G * B)))
      else
          H := 360.0 - RadToDeg(ArcCos((R - G * 0.5 - B * 0.5) / Sqrt(Sqr(R) + Sqr(G) + Sqr(B) - R * G - R * B - G * B)))
    end
  else
      H := 361.0;

  H := (H + 1) / 363.0;
end;

function THSI.GetRGBA(A: Byte): TRColor;
begin
  Result := GetRGB();
  TRColorEntry(Result).A := A;
end;

procedure THSI.SetRGBA(A: Byte; const Value: TRColor);
begin
  SetRGB(Value);
end;

function TCMYK.GetRGB: TRColor;
var
  R, G, B: TGeoFloat;
begin
  R := (1.0 - C) * (1.0 - K);
  G := (1.0 - M) * (1.0 - K);
  B := (1.0 - Y) * (1.0 - K);
  Result := RColorF(R, G, B);
end;

procedure TCMYK.SetRGB(const Value: TRColor);
var
  R, G, B: TGeoFloat;
begin
  RColor2F(Value, R, G, B);
  K := 1.0 - umlMax(umlMax(R, G), B);
  if K <> 1.0 then
    begin
      C := (1.0 - R - K) / (1.0 - K);
      M := (1.0 - G - K) / (1.0 - K);
      Y := (1.0 - B - K) / (1.0 - K);
    end
  else
    begin
      C := 1.0;
      M := 1.0;
      Y := 1.0;
    end;
end;

function TCMYK.GetRGBA(A: Byte): TRColor;
begin
  Result := GetRGB();
  TRColorEntry(Result).A := A;
end;

procedure TCMYK.SetRGBA(A: Byte; const Value: TRColor);
begin
  SetRGB(Value);
end;

function TMemoryRaster.GetExtra: THashStringList;
begin
  if FExtra = nil then
      FExtra := THashStringList.CustomCreate(128);
  Result := FExtra;
end;

function TMemoryRaster.GetVertex: TRasterVertex;
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

function TMemoryRaster.GetPixel(const X, Y: Integer): TRColor;
begin
  Result := PixelPtr[X, Y]^;
end;

procedure TMemoryRaster.SetPixel(const X, Y: Integer; const Value: TRColor);
begin
  PixelPtr[X, Y]^ := Value;
end;

function TMemoryRaster.GetFastPixel(const X, Y: Integer): TRColor;
begin
  Result := FBits^[X + Y * Width];
end;

procedure TMemoryRaster.SetFastPixel(const X, Y: Integer; const Value: TRColor);
begin
  FBits^[X + Y * Width] := Value;
end;

function TMemoryRaster.GetPixelBGRA(const X, Y: Integer): TRColor;
begin
  Result := RGBA2BGRA(GetPixel(X, Y));
end;

procedure TMemoryRaster.SetPixelBGRA(const X, Y: Integer; const Value: TRColor);
begin
  SetPixel(X, Y, BGRA2RGBA(Value));
end;

function TMemoryRaster.GetPixelPtr(const X, Y: Integer): PRColor;
begin
  Result := @(Bits^[ClampInt(X, 0, Width - 1) + ClampInt(Y, 0, Height - 1) * Width]);
end;

function TMemoryRaster.GetScanLine(Y: Integer): PRColorArray;
begin
  Result := @Bits^[Y * FWidth];
end;

function TMemoryRaster.GetWidth0: TGeoFloat;
begin
  if FWidth > 0 then
      Result := FWidth - 1
  else
      Result := 0;
end;

function TMemoryRaster.GetHeight0: TGeoFloat;
begin
  if FHeight > 0 then
      Result := FHeight - 1
  else
      Result := 0;
end;

function TMemoryRaster.GetWidth0i: Integer;
begin
  if FWidth > 0 then
      Result := FWidth - 1
  else
      Result := 0;
end;

function TMemoryRaster.GetHeight0i: Integer;
begin
  if FHeight > 0 then
      Result := FHeight - 1
  else
      Result := 0;
end;

function TMemoryRaster.GetPixelRed(const X, Y: Integer): Byte;
begin
  Result := PRColorEntry(GetPixelPtr(X, Y))^.R;
end;

procedure TMemoryRaster.SetPixelRed(const X, Y: Integer; const Value: Byte);
begin
  PRColorEntry(GetPixelPtr(X, Y))^.R := Value;
end;

function TMemoryRaster.GetPixelGreen(const X, Y: Integer): Byte;
begin
  Result := PRColorEntry(GetPixelPtr(X, Y))^.G;
end;

procedure TMemoryRaster.SetPixelGreen(const X, Y: Integer; const Value: Byte);
begin
  PRColorEntry(GetPixelPtr(X, Y))^.G := Value;
end;

function TMemoryRaster.GetPixelBlue(const X, Y: Integer): Byte;
begin
  Result := PRColorEntry(GetPixelPtr(X, Y))^.B;
end;

procedure TMemoryRaster.SetPixelBlue(const X, Y: Integer; const Value: Byte);
begin
  PRColorEntry(GetPixelPtr(X, Y))^.B := Value;
end;

function TMemoryRaster.GetPixelAlpha(const X, Y: Integer): Byte;
begin
  Result := PRColorEntry(GetPixelPtr(X, Y))^.A;
end;

procedure TMemoryRaster.SetPixelAlpha(const X, Y: Integer; const Value: Byte);
begin
  PRColorEntry(GetPixelPtr(X, Y))^.A := Value;
end;

function TMemoryRaster.GetGray(const X, Y: Integer): Byte;
begin
  Result := RColor2Gray(GetPixel(X, Y));
end;

procedure TMemoryRaster.SetGray(const X, Y: Integer; const Value: Byte);
begin
  SetPixel(X, Y, RColor(Value, Value, Value, $FF));
end;

function TMemoryRaster.GetGrayS(const X, Y: Integer): TGeoFloat;
begin
  Result := RColor2GrayS(GetPixel(X, Y));
end;

procedure TMemoryRaster.SetGrayS(const X, Y: Integer; const Value: TGeoFloat);
begin
  SetGray(X, Y, ClampByte3(Round(Value * $FF), 0, $FF));
end;

function TMemoryRaster.GetGrayD(const X, Y: Integer): Double;
begin
  Result := RColor2GrayD(GetPixel(X, Y));
end;

procedure TMemoryRaster.SetGrayD(const X, Y: Integer; const Value: Double);
begin
  SetGrayS(X, Y, Value);
end;

function TMemoryRaster.GetPixelF(const X, Y: TGeoFloat): TRColor;
begin
  Result := GetPixel(Round(X), Round(Y));
end;

procedure TMemoryRaster.SetPixelF(const X, Y: TGeoFloat; const Value: TRColor);
begin
  SetPixel(Round(X), Round(Y), Value);
end;

function TMemoryRaster.GetPixelVec(const v2: TVec2): TRColor;
begin
  Result := GetPixelF(v2[0], v2[1]);
end;

procedure TMemoryRaster.SetPixelVec(const v2: TVec2; const Value: TRColor);
begin
  SetPixelF(v2[0], v2[1], Value)
end;

function TMemoryRaster.GetPixelLinearMetric(const X, Y: TGeoFloat): TRColor;
var
  fx, fy: TGeoFloat;
  i_x, i_y: Integer;
  i_x2, i_y2: Integer;
  delta_x, delta_y: TGeoFloat;
  c1, c2, c3, c4: TRColorEntry;
  k1, k2, k3, K4: TGeoFloat;
  R, G, B, A: TGeoFloat;
begin
  fx := umlClamp(X, 0.0, 1.0) * Width0;
  fy := umlClamp(Y, 0.0, 1.0) * Height0;

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

  R := ((c1.R / $FF) * k1) + ((c2.R / $FF) * k2) + ((c3.R / $FF) * k3) + ((c4.R / $FF) * K4);
  G := ((c1.G / $FF) * k1) + ((c2.G / $FF) * k2) + ((c3.G / $FF) * k3) + ((c4.G / $FF) * K4);
  B := ((c1.B / $FF) * k1) + ((c2.B / $FF) * k2) + ((c3.B / $FF) * k3) + ((c4.B / $FF) * K4);
  A := ((c1.A / $FF) * k1) + ((c2.A / $FF) * k2) + ((c3.A / $FF) * k3) + ((c4.A / $FF) * K4);

  Result := RColorF(R, G, B, A);
end;

function TMemoryRaster.GetPixelLinear(const X, Y: Integer): TRColor;
begin
  Result := GetPixelLinearMetric(X / Width0, Y / Height0);
end;

function TMemoryRaster.GetPixelYIQ(const X, Y: Integer): TYIQ;
begin
  Result.RGB := Pixel[X, Y];
end;

procedure TMemoryRaster.SetPixelYIQ(const X, Y: Integer; const Value: TYIQ);
begin
  Pixel[X, Y] := Value.RGB;
end;

function TMemoryRaster.GetPixelHSI(const X, Y: Integer): THSI;
begin
  Result.RGB := Pixel[X, Y];
end;

procedure TMemoryRaster.SetPixelHSI(const X, Y: Integer; const Value: THSI);
begin
  Pixel[X, Y] := Value.RGB;
end;

function TMemoryRaster.GetPixelCMYK(const X, Y: Integer): TCMYK;
begin
  Result.RGB := Pixel[X, Y];
end;

procedure TMemoryRaster.SetPixelCMYK(const X, Y: Integer; const Value: TCMYK);
begin
  Pixel[X, Y] := Value.RGB;
end;

constructor TMemoryRaster.Create;
begin
  inherited Create;

  LocalParallel := True;

  FDrawEngineMap := nil;

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
  FExtra := nil;
end;

destructor TMemoryRaster.Destroy;
begin
  if FDrawEngineMap <> nil then
      DisposeObject(FDrawEngineMap);
  if FSerializedEngine <> nil then
      FSerializedEngine.Remove(Self);
  CloseFont;
  CloseVertex;
  CloseAgg;
  RecycleMemory;
  if FExtra <> nil then
      DisposeObject(FExtra);
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
  if (FSerializedEngine <> nil) and (FSerializedEngine <> RSeri) and empty() then
      UnserializedMemory();

  if FSerializedEngine <> RSeri then
      FMemorySerializedPosition := -1;

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
  if FSerializedEngine <> RSeri then
      FMemorySerializedPosition := -1;
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

procedure TMemoryRaster.SetWorkMemory(Forever: Boolean; WorkMemory: Pointer; NewWidth, NewHeight: Integer);
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
      FFreeBits := Forever;
      FBits := PRColorArray(WorkMemory);
      FWidth := NewWidth;
      FHeight := NewHeight;
    end;

  if FAggNeed then
      OpenAgg;

  FMemorySerializedPosition := -1;
  FSerializedEngine := nil;
end;

procedure TMemoryRaster.SetWorkMemory(WorkMemory: Pointer; NewWidth, NewHeight: Integer);
begin
  SetWorkMemory(False, WorkMemory, NewWidth, NewHeight);
end;

procedure TMemoryRaster.SetWorkMemory(Forever: Boolean; raster: TMemoryRaster);
begin
  SetWorkMemory(Forever, @raster.Bits^[0], raster.Width, raster.Height);
  if Forever then
      raster.FBits := nil;
end;

procedure TMemoryRaster.SetWorkMemory(raster: TMemoryRaster);
begin
  SetWorkMemory(False, raster);
end;

function TMemoryRaster.IsMemoryMap: Boolean;
begin
  Result := (FWidth > 0) and (FHeight > 0) and (not FFreeBits);
end;

procedure TMemoryRaster.OpenVertex;
begin
  if FVertex = nil then
    begin
      FVertex := TRasterVertex.Create(Self);
      FVertex.LocalParallel := LocalParallel;
    end;
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
  if FFont = nil then
      FFont := TFontRaster.Create(Wait_SystemFont_Init);
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

procedure TMemoryRaster.NoUsage;
begin
end;

procedure TMemoryRaster.Update;
begin
  NoUsage;
end;

procedure TMemoryRaster.DiscardMemory;
begin
  if FFont <> nil then
    begin
      DisposeObject(FFont);
      FFont := nil;
    end;

  CloseVertex;
  FreeAgg;
  FBits := nil;
  FFreeBits := True;
  FWidth := 0;
  FHeight := 0;

  FMemorySerializedPosition := -1;
  FSerializedEngine := nil;
end;

procedure TMemoryRaster.SwapInstance(dest: TMemoryRaster);
var
  bak_Bits: PRColorArray;
  bak_Width, bak_Height: Integer;
  bak_FreeBits: Boolean;
  bak_SerializedEngine: TRasterSerialized;
  bak_MemorySerializedPosition: Int64;
begin
  if Self = dest then
      exit;
  ReadyBits;
  CloseVertex;
  FreeAgg;

  dest.ReadyBits;
  dest.CloseVertex;
  dest.FreeAgg;

  bak_Bits := FBits;
  bak_Width := FWidth;
  bak_Height := FHeight;
  bak_FreeBits := FFreeBits;
  bak_SerializedEngine := FSerializedEngine;
  bak_MemorySerializedPosition := FMemorySerializedPosition;

  FBits := dest.FBits;
  FWidth := dest.FWidth;
  FHeight := dest.FHeight;
  FFreeBits := dest.FFreeBits;
  FSerializedEngine := dest.FSerializedEngine;
  FMemorySerializedPosition := dest.FMemorySerializedPosition;

  dest.FBits := bak_Bits;
  dest.FWidth := bak_Width;
  dest.FHeight := bak_Height;
  dest.FFreeBits := bak_FreeBits;
  dest.FSerializedEngine := bak_SerializedEngine;
  dest.FMemorySerializedPosition := bak_MemorySerializedPosition;
end;

function TMemoryRaster.BitsSame(sour: TMemoryRaster): Boolean;
begin
  Result := (Bits = sour.Bits) and (FWidth = sour.FWidth) and (FHeight = sour.FHeight);
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

function TMemoryRaster.Clone: TMemoryRaster;
begin
  Result := NewRaster();
  Result.Assign(Self);
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

procedure TMemoryRaster.Assign(sour: TMorphologyBinaryzation);
var
  i: Integer;
begin
  Reset;
  FWidth := sour.FWidth;
  FHeight := sour.FHeight;

  FOuterColor := $00000000; // by default as full transparency black
  FMasterAlpha := $FF;
  FDrawMode := dmBlend;
  FCombineMode := cmBlend;

  CloseVertex;
  FreeAgg;

  FFreeBits := True;
  FAggNeed := False;

  FBits := System.GetMemory(sour.FWidth * sour.FHeight * SizeOf(TRColor));
  for i := Width * Height - 1 downto 0 do
    if sour.FBits^[i] then
        FBits^[i] := $FFFFFFFF
    else
        FBits^[i] := $00000000;

  FMemorySerializedPosition := -1;
  FSerializedEngine := nil;
end;

procedure TMemoryRaster.Assign(sour: TMorphomatics);
var
  i: Integer;
begin
  Reset;
  FWidth := sour.FWidth;
  FHeight := sour.FHeight;

  FOuterColor := $00000000; // by default as full transparency black
  FMasterAlpha := $FF;
  FDrawMode := dmBlend;
  FCombineMode := cmBlend;

  CloseVertex;
  FreeAgg;

  FFreeBits := True;
  FAggNeed := False;

  FBits := System.GetMemory(sour.FWidth * sour.FHeight * SizeOf(TRColor));
  for i := Width * Height - 1 downto 0 do
      MorphToRColor(mpGrayscale, sour.FBits^[i], FBits^[i]);

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
  FillRColor(@Bits^[0], Width * Height, FillColor);
end;

function TMemoryRaster.MemorySize: Integer;
begin
  Result := Width * Height * SizeOf(TRColor);
end;

function TMemoryRaster.GetMD5: TMD5;
begin
  Result := umlMD5(@Bits^[0], MemorySize);
end;

function TMemoryRaster.GetCRC32: Cardinal;
begin
  Result := umlCRC32(@Bits^[0], MemorySize);
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
  FillRColor(@Bits^[0], NewWidth * NewHeight, ClearColor);
end;

procedure TMemoryRaster.SetSizeF(NewWidth, NewHeight: TGeoFloat; const ClearColor: TRColor);
begin
  SetSize(Round(NewWidth), Round(NewHeight), ClearColor);
end;

procedure TMemoryRaster.SetSizeF(NewWidth, NewHeight: TGeoFloat);
begin
  SetSize(Round(NewWidth), Round(NewHeight));
end;

procedure TMemoryRaster.SetSizeR(R: TRectV2; const ClearColor: TRColor);
begin
  SetSizeF(RectWidth(R), RectHeight(R), ClearColor);
end;

procedure TMemoryRaster.SetSizeR(R: TRectV2);
begin
  SetSizeF(RectWidth(R), RectHeight(R));
end;

procedure TMemoryRaster.SetSizeR(R: TRect; const ClearColor: TRColor);
begin
  SetSizeR(RectV2(R), ClearColor);
end;

procedure TMemoryRaster.SetSizeR(R: TRect);
begin
  SetSizeR(RectV2(R));
end;

function TMemoryRaster.SizeOfPoint: TPoint;
begin
  Result := Point(Width, Height);
end;

function TMemoryRaster.SizeOf2DPoint: TVec2;
begin
  Result := Make2DPoint(Width, Height);
end;

function TMemoryRaster.Size2D: TVec2;
begin
  Result := Make2DPoint(Width, Height);
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
  Result.Right := Width;
  Result.Bottom := Height;
end;

function TMemoryRaster.BoundsRect0: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Width0i;
  Result.Bottom := Height0i;
end;

function TMemoryRaster.BoundsRectV2: TRectV2;
begin
  Result := MakeRectV2(0, 0, Width, Height);
end;

function TMemoryRaster.BoundsRectV20: TRectV2;
begin
  Result := MakeRectV2(0, 0, Width0, Height0);
end;

function TMemoryRaster.BoundsV2Rect4: TV2Rect4;
begin
  Result := TV2Rect4.Init(BoundsRectV2, 0);
end;

function TMemoryRaster.BoundsV2Rect40: TV2Rect4;
begin
  Result := TV2Rect4.Init(BoundsRectV20, 0);
end;

function TMemoryRaster.Centroid: TVec2;
begin
  Result := Vec2(Width * 0.5, Height * 0.5);
end;

function TMemoryRaster.Centre: TVec2;
begin
  Result := Vec2(Width * 0.5, Height * 0.5);
end;

function TMemoryRaster.InHere(const X, Y: Integer): Boolean;
begin
  Result := PointInRect(X, Y, 0, 0, Width - 1, Height - 1);
end;

procedure TMemoryRaster.FlipHorz;
var
  i, j: Integer;
  p1, p2: PRColor;
  tmp: TRColor;
  W, W2: Integer;
begin
  CloseVertex;
  FreeAgg;

  W := Width;
  { In-place flipping }
  p1 := PRColor(Bits);
  p2 := p1;
  inc(p2, Width - 1);
  W2 := Width shr 1;
  for j := 0 to Height - 1 do
    begin
      for i := 0 to W2 - 1 do
        begin
          tmp := p1^;
          p1^ := p2^;
          p2^ := tmp;
          inc(p1);
          dec(p2);
        end;
      inc(p1, W - W2);
      inc(p2, W + W2);
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
  j2 := Height - 1;
  buffer := System.GetMemory(Width shl 2);
  for j := 0 to Height div 2 - 1 do
    begin
      p1 := PixelPtr[0, j];
      p2 := PixelPtr[0, j2];
      CopyRColor(p1^, buffer^, Width);
      CopyRColor(p2^, p1^, Width);
      CopyRColor(buffer^, p2^, Width);
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
  X, Y, i, j: Integer;
begin
  CloseVertex;
  FreeAgg;

  ReadyBits();

  tmp := TMemoryRaster.Create;

  tmp.SetSize(Height, Width);
  i := 0;
  for Y := 0 to Height - 1 do
    begin
      j := Height - 1 - Y;
      for X := 0 to Width - 1 do
        begin
          tmp.FBits^[j] := FBits^[i];
          inc(i);
          inc(j, Height);
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
  i2 := Width * Height - 1;
  for i := 0 to Width * Height div 2 - 1 do
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
  X, Y, i, j: Integer;
begin
  CloseVertex;
  FreeAgg;

  ReadyBits();

  tmp := TMemoryRaster.Create;

  tmp.SetSize(Height, Width);
  i := 0;
  for Y := 0 to Height - 1 do
    begin
      j := (Width - 1) * Height + Y;
      for X := 0 to Width - 1 do
        begin
          tmp.FBits^[j] := FBits^[i];
          inc(i);
          dec(j, Height);
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

function TMemoryRaster.Rotate(dest: TMemoryRaster; Angle: TGeoFloat; Edge: Integer): TV2R4;
begin
  Result := TV2Rect4.Init(BoundsRectV20(), Angle).TransformToRect(dest.BoundsRectV20(), Edge);
  ProjectionTo(dest, BoundsV2Rect40(), Result, True, 1.0);
end;

function TMemoryRaster.Rotate(Angle: TGeoFloat; Edge: Integer; BackgroundColor: TRColor): TV2R4;
var
  n: TMemoryRaster;
  V: TV2Rect4;
  R: TRectV2;
begin
  CloseVertex;
  FreeAgg;

  V := TV2Rect4.Init(BoundsRectV20(), Angle);
  R := V.BoundRect;

  n := TMemoryRaster.Create;
  n.SetSizeF(RectWidth(R) + Edge * 2, RectHeight(R) + Edge * 2, BackgroundColor);
  Result := Rotate(n, Angle, Edge);
  Reset;
  FWidth := n.Width;
  FHeight := n.Height;
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

function TMemoryRaster.Rotate(dest: TMemoryRaster; Axis: TVec2; Angle: TGeoFloat; Edge: Integer): TV2R4;
begin
  Result := TV2Rect4.Init(BoundsRectV20, Axis, Angle).TransformToRect(dest.BoundsRectV20, Edge);
  ProjectionTo(dest, BoundsV2Rect40, Result, True, 1.0);
end;

function TMemoryRaster.Rotate(Axis: TVec2; Angle: TGeoFloat; Edge: Integer; BackgroundColor: TRColor): TV2R4;
var
  n: TMemoryRaster;
  V: TV2Rect4;
  R: TRectV2;
begin
  CloseVertex;
  FreeAgg;

  V := TV2Rect4.Init(BoundsRectV20, Axis, Angle);
  R := V.BoundRect;

  n := TMemoryRaster.Create;
  n.SetSizeF(RectWidth(R) + Edge * 2, RectHeight(R) + Edge * 2, BackgroundColor);
  Result := Rotate(n, Angle, Edge);
  Reset;
  FWidth := n.Width;
  FHeight := n.Height;
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

procedure TMemoryRaster.CalibrateRotate_LineDistance(BackgroundColor: TRColor);
var
  BestLines_: THoughLineArry;
begin
  if empty then
      exit;
  BestLines_ := BuildRasterHoughLine(45, 0.1, 50, Self);
  Rotate(-DocumentRotationDetected_MaxDistance(BestLines_), 0, BackgroundColor);
  SetLength(BestLines_, 0);
end;

procedure TMemoryRaster.CalibrateRotate_LineMatched(BackgroundColor: TRColor);
var
  BestLines_: THoughLineArry;
begin
  if empty then
      exit;
  BestLines_ := BuildRasterHoughLine(45, 0.1, 50, Self);
  Rotate(-DocumentRotationDetected_MaxMatched(BestLines_), 0, BackgroundColor);
  SetLength(BestLines_, 0);
end;

procedure TMemoryRaster.CalibrateRotate_AVG(BackgroundColor: TRColor);
var
  BestLines_: THoughLineArry;
begin
  if empty then
      exit;
  BestLines_ := BuildRasterHoughLine(45, 0.1, 50, Self);
  Rotate(-DocumentRotationDetected_AVG(BestLines_), 0, BackgroundColor);
  SetLength(BestLines_, 0);
end;

procedure TMemoryRaster.CalibrateRotate(BackgroundColor: TRColor);
begin
  CalibrateRotate_AVG(BackgroundColor);
end;

procedure TMemoryRaster.CalibrateRotate;
begin
  CalibrateRotate(RColor(0, 0, 0, 0));
end;

procedure TMemoryRaster.NonlinearZoomLine(const Source, dest: TMemoryRaster; const pass: Integer);
var
  j: Integer;
  SourceI, SourceJ: Double;
  SourceIInt, SourceJInt: Integer;
  SourceINext, SourceJNext: Integer;
begin
  for j := 0 to dest.Height - 1 do
    begin
      SourceI := pass / dest.Width0 * Source.Width0;
      SourceJ := j / dest.Height0 * Source.Height0;

      SourceIInt := Trunc(SourceI);
      SourceJInt := Trunc(SourceJ);

      dest.Pixel[pass, j] := Source.Pixel[SourceIInt, SourceJInt]
    end;
end;

procedure TMemoryRaster.NonlinearZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    NonlinearZoomLine(Source, Self, pass);
  end;
{$ENDIF FPC}
{$ENDIF Parallel}
  procedure DoFor();
  var
    i: Integer;
  begin
    for i := Width - 1 downto 0 do
        NonlinearZoomLine(Source, Self, i);
  end;

begin
  if (Source.Width = NewWidth) and (Source.Height = NewHeight) then
    begin
      Assign(Source);
      exit;
    end;
  SetSize(NewWidth, NewHeight);

  if (Source.Width > 1) and (Source.Width > 1) and (Width > 1) and (Height > 1) then
    begin
      if NewHeight * NewWidth > 800 * 800 then
        begin
{$IFDEF Parallel}
{$IFDEF FPC}
          FPCParallelFor(TMemoryRaster.Parallel and LocalParallel, @Nested_ParallelFor, 0, Width - 1);
{$ELSE}
          DelphiParallelFor(TMemoryRaster.Parallel and LocalParallel, 0, Width - 1, procedure(pass: Integer)
            begin
              NonlinearZoomLine(Source, Self, pass);
            end);
{$ENDIF FPC}
{$ELSE}
          DoFor();
{$ENDIF Parallel}
        end
      else
          DoFor();
    end;
end;

procedure TMemoryRaster.NonlinearZoom(const NewWidth, NewHeight: Integer);
var
  n: TMemoryRaster;
begin
  CloseVertex;
  FreeAgg;

  n := TMemoryRaster.Create;
  n.NonlinearZoomFrom(Self, NewWidth, NewHeight);
  Reset;
  FWidth := n.Width;
  FHeight := n.Height;
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
  for j := 0 to dest.Height - 1 do
    begin
      SourceI := pass / dest.Width0;
      SourceJ := j / dest.Height0;

      dest.Pixel[pass, j] := Source.PixelLinearMetric[SourceI, SourceJ];
    end;
end;

procedure TMemoryRaster.ZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    ZoomLine(Source, Self, pass);
  end;
{$ENDIF FPC}
{$ENDIF Parallel}
  procedure DoFor();
  var
    i: Integer;
  begin
    for i := Width - 1 downto 0 do
        ZoomLine(Source, Self, i);
  end;

begin
  if (Source.Width = NewWidth) and (Source.Height = NewHeight) then
    begin
      Assign(Source);
      exit;
    end;
  SetSize(NewWidth, NewHeight);

  if (Source.Width > 1) and (Source.Width > 1) and (Width > 1) and (Height > 1) then
    begin
      if NewHeight * NewWidth > 800 * 800 then
        begin
{$IFDEF Parallel}
{$IFDEF FPC}
          FPCParallelFor(TMemoryRaster.Parallel and LocalParallel, @Nested_ParallelFor, 0, Width - 1);
{$ELSE}
          DelphiParallelFor(TMemoryRaster.Parallel and LocalParallel, 0, Width - 1, procedure(pass: Integer)
            begin
              ZoomLine(Source, Self, pass);
            end);
{$ENDIF FPC}
{$ELSE}
          DoFor();
{$ENDIF Parallel}
        end
      else
          DoFor();
    end;
end;

procedure TMemoryRaster.ZoomFrom(const Source: TMemoryRaster; const f: TGeoFloat);
begin
  ZoomFrom(Source, Round(Source.Width * f), Round(Source.Height * f));
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
  FWidth := n.Width;
  FHeight := n.Height;
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
  if (Source.Width = NewWidth) and (Source.Height = NewHeight) then
    begin
      Assign(Source);
      exit;
    end;
  k1 := Max(NewWidth / Source.Width, NewHeight / Source.Height);
  k2 := Max(Source.Width / NewWidth, Source.Height / NewHeight);
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
  FWidth := n.Width;
  FHeight := n.Height;
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
  if (Source.Width = NewWidth) and (Source.Height = NewHeight) then
    begin
      Assign(Source);
      exit;
    end;

  k1 := Max(NewWidth / Source.Width, NewHeight / Source.Height);
  k2 := Max(Source.Width / NewWidth, Source.Height / NewHeight);
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
  FWidth := n.Width;
  FHeight := n.Height;
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
  if (Source.Width = NewWidth) and (Source.Height = NewHeight) then
    begin
      Assign(Source);
      Grayscale;
      exit;
    end;
  k1 := Max(NewWidth / Source.Width, NewHeight / Source.Height);
  k2 := Max(Source.Width / NewWidth, Source.Height / NewHeight);
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
  FWidth := n.Width;
  FHeight := n.Height;
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

procedure TMemoryRaster.Scale(K: TGeoFloat);
begin
  Zoom(Round(Width * K), Round(Height * K));
end;

procedure TMemoryRaster.FastBlurScale(K: TGeoFloat);
begin
  FastBlurZoom(Round(Width * K), Round(Height * K));
end;

procedure TMemoryRaster.GaussianBlurScale(K: TGeoFloat);
begin
  GaussianBlurZoom(Round(Width * K), Round(Height * K));
end;

procedure TMemoryRaster.NonlinearScale(K: TGeoFloat);
begin
  NonlinearZoom(Round(Width * K), Round(Height * K));
end;

procedure TMemoryRaster.FitScale(NewWidth, NewHeight: TGeoFloat);
var
  R: TRectV2;
begin
  R := FitRect(BoundsRectV2, RectV2(0, 0, NewWidth, NewHeight));
  Zoom(Round(RectWidth(R)), Round(RectHeight(R)));
end;

procedure TMemoryRaster.FitScale(R: TRectV2);
var
  R_: TRectV2;
begin
  R_ := FitRect(BoundsRectV2, R);
  Zoom(Round(RectWidth(R_)), Round(RectHeight(R_)));
end;

function TMemoryRaster.FitScaleAsNew(NewWidth, NewHeight: TGeoFloat): TMemoryRaster;
var
  R: TRectV2;
begin
  R := FitRect(BoundsRectV2, RectV2(0, 0, NewWidth, NewHeight));
  Result := NewRaster();
  Result.ZoomFrom(Self, Round(RectWidth(R)), Round(RectHeight(R)));
end;

function TMemoryRaster.FitScaleAsNew(R: TRectV2): TMemoryRaster;
var
  R_: TRectV2;
begin
  R_ := FitRect(BoundsRectV2, R);
  Result := NewRaster();
  Result.ZoomFrom(Self, Round(RectWidth(R_)), Round(RectHeight(R_)));
end;

function TMemoryRaster.NonlinearFitScaleAsNew(NewWidth, NewHeight: TGeoFloat): TMemoryRaster;
var
  R: TRectV2;
begin
  R := FitRect(BoundsRectV2, RectV2(0, 0, NewWidth, NewHeight));
  Result := NewRaster();
  Result.NonlinearZoomFrom(Self, Round(RectWidth(R)), Round(RectHeight(R)));
end;

function TMemoryRaster.NonlinearFitScaleAsNew(R: TRectV2): TMemoryRaster;
var
  R_: TRectV2;
begin
  R_ := FitRect(BoundsRectV2, R);
  Result := NewRaster();
  Result.NonlinearZoomFrom(Self, Round(RectWidth(R_)), Round(RectHeight(R_)));
end;

procedure TMemoryRaster.SigmaGaussian(const SIGMA: TGeoFloat; const SigmaGaussianKernelFactor: Integer);
begin
  PixelSigmaGaussianSampler(True, Self, Self, SIGMA, SigmaGaussianKernelFactor);
end;

procedure TMemoryRaster.SigmaGaussian(const SIGMA: TGeoFloat);
begin
  SigmaGaussian(True, SIGMA, 3);
end;

procedure TMemoryRaster.SigmaGaussian(parallel_: Boolean; const SIGMA: TGeoFloat; const SigmaGaussianKernelFactor: Integer);
begin
  PixelSigmaGaussianSampler(parallel_, Self, Self, SIGMA, SigmaGaussianKernelFactor);
end;

procedure TMemoryRaster.SigmaGaussian(parallel_: Boolean; const SIGMA: TGeoFloat);
begin
  SigmaGaussian(parallel_, SIGMA, 3);
end;

function TMemoryRaster.FormatAsBGRA: TMemoryRaster;
var
  dest: TMemoryRaster;
  pass: Integer;
begin
  ReadyBits();
  dest := TMemoryRaster.Create;
  dest.SetSize(Width, Height);
  for pass := (Width * Height) - 1 downto 0 do
      dest.FBits^[pass] := RGBA2BGRA(FBits^[pass]);

  Result := dest;
end;

procedure TMemoryRaster.FormatBGRA;
var
  pass: Integer;
begin
  ReadyBits();
  for pass := (Width * Height) - 1 downto 0 do
      SwapBR(FBits^[pass]);
end;

function TMemoryRaster.BuildRGB(cSwapBR: Boolean): PRGBArray;
begin
  Result := System.GetMemory(Width * Height * 3);
  OutputRGB(Result^, cSwapBR);
end;

procedure TMemoryRaster.InputRGB(var buff; W, H: Integer; cSwapBR: Boolean);
var
  pass: Integer;
  p: PByte;
begin
  if (Width <> W) or (Height <> H) then
      SetSize(W, H);

  ReadyBits();
  p := @buff;
  for pass := 0 to (Width * Height) - 1 do
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
  for pass := 0 to (Width * Height) - 1 do
    begin
      PRGB(p)^ := PRGB(@FBits^[pass])^;
      if cSwapBR then
          SwapBR(PRGB(p)^);
      inc(p, 3);
    end;
end;

function TMemoryRaster.EncryptGrayscale(): PByteBuffer;
var
  siz: NativeInt;
  p: PByteBuffer;
  i: Integer;
begin
  siz := Width * Height;
  p := System.GetMemory(siz);
  ReadyBits();
  for i := 0 to siz - 1 do
      p^[i] := RColor2Gray(DirectBits^[i]);
  Result := p;
end;

function TMemoryRaster.EncryptColor255(): PByteBuffer;
var
  siz: NativeInt;
  p: PByteBuffer;
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    p^[pass] := FindColor255Index(DirectBits^[pass]);
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    pass: Integer;
  begin
    for pass := 0 to siz - 1 do
      begin
        p^[pass] := FindColor255Index(DirectBits^[pass]);
      end;
  end;
{$ENDIF Parallel}


begin
  siz := Width * Height;
  p := System.GetMemory(siz);

  ReadyBits();

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMemoryRaster.Parallel and LocalParallel, @Nested_ParallelFor, 0, siz - 1);
{$ELSE FPC}
  DelphiParallelFor(TMemoryRaster.Parallel and LocalParallel, 0, siz - 1, procedure(pass: Integer)
    begin
      p^[pass] := FindColor255Index(DirectBits^[pass]);
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  Result := p;
end;

function TMemoryRaster.EncryptColor65535(): PWordBuffer;
var
  siz: NativeInt;
  p: PWordBuffer;
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    p^[pass] := FindColor65535Index(DirectBits^[pass]);
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    pass: Integer;
  begin
    for pass := 0 to siz - 1 do
      begin
        p^[pass] := FindColor65535Index(DirectBits^[pass]);
      end;
  end;
{$ENDIF Parallel}


begin
  siz := Width * Height;
  p := System.GetMemory(siz * 2);

  ReadyBits();

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMemoryRaster.Parallel and LocalParallel, @Nested_ParallelFor, 0, siz - 1);
{$ELSE FPC}
  DelphiParallelFor(TMemoryRaster.Parallel and LocalParallel, 0, siz - 1, procedure(pass: Integer)
    begin
      p^[pass] := FindColor65535Index(DirectBits^[pass]);
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  Result := p;
end;

procedure TMemoryRaster.DecryptGrayscale(Width_, Height_: Integer; buffer: PByteBuffer);
var
  i: Integer;
begin
  SetSize(Width_, Height_);
  for i := 0 to Width_ * Height_ - 1 do
      DirectBits^[i] := RColor(buffer^[i], buffer^[i], buffer^[i]);
end;

procedure TMemoryRaster.DecryptColor255(Width_, Height_: Integer; buffer: PByteBuffer);
var
  i: Integer;
begin
  SetSize(Width_, Height_);
  for i := 0 to Width_ * Height_ - 1 do
      DirectBits^[i] := Color255[buffer^[i]];
end;

procedure TMemoryRaster.DecryptColor65535(Width_, Height_: Integer; buffer: PWordBuffer);
var
  i: Integer;
begin
  SetSize(Width_, Height_);
  for i := 0 to Width_ * Height_ - 1 do
      DirectBits^[i] := Color65535[buffer^[i]];
end;

procedure TMemoryRaster.ColorReplace(const old_c, new_c: TRColor);
var
  pass: Integer;
  p: PRColor;
begin
  pass := Width * Height;
  if pass <= 0 then
      exit;
  ReadyBits();
  p := @FBits^[0];
  while pass > 0 do
    begin
      if p^ = old_c then
          p^ := new_c;
      inc(p);
      dec(pass);
    end;
end;

procedure TMemoryRaster.ColorTransparent(c_: TRColor);
var
  pass: Integer;
  p: PRColor;
  C: TRColor;
begin
  pass := Width * Height;
  if pass <= 0 then
      exit;
  C := RasterAlphaColor(c_, 0);
  ReadyBits();
  p := @FBits^[0];
  while pass > 0 do
    begin
      if RasterAlphaColor(p^, 0) = C then
          p^ := 0;
      inc(p);
      dec(pass);
    end;
end;

procedure TMemoryRaster.ColorBlend(C: TRColor);
var
  pass: Integer;
  p: PRColor;
begin
  pass := Width * Height;
  if pass <= 0 then
      exit;
  ReadyBits();
  p := @FBits^[0];
  while pass > 0 do
    begin
      p^ := BlendReg(p^, C);
      inc(p);
      dec(pass);
    end;
end;

procedure TMemoryRaster.Grayscale;
var
  i: Integer;
  p: PRColorEntry;
begin
  ReadyBits();
  for i := (Width * Height) - 1 downto 0 do
    begin
      p := @FBits^[i];
      p^.R := RColor2Gray(p^.BGRA);
      p^.G := p^.R;
      p^.B := p^.R;
    end;
end;

procedure TMemoryRaster.Gradient(level: Byte);
var
  i: Integer;
  p: PRColorEntry;
  f: TGeoFloat;
begin
  if level = $FF then
      Grayscale
  else
    begin
      f := $FF / level;
      ReadyBits();
      for i := (Width * Height) - 1 downto 0 do
        begin
          p := @FBits^[i];
          p^.R := Round(Trunc(RColor2Gray(p^.BGRA) / f) * f);
          p^.G := p^.R;
          p^.B := p^.R;
        end;
    end;
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

function TMemoryRaster.ComputeAreaScaleSpace(clipArea: TRectV2; SS_width, SS_height: TGeoFloat): TRectV2;
var
  K: TGeoFloat;
  R: TRectV2;
  W, H, nW, nH: TGeoFloat;
  d: TVec2;
begin
  R := FixRect(clipArea);
  W := RectWidth(R);
  H := RectHeight(R);

  if W < H then
    begin
      K := SS_width / SS_height;
      nW := H * K;
      nH := H;
    end
  else
    begin
      K := SS_height / SS_width;
      nW := W;
      nH := W * K;
    end;

  d[0] := (nW - W) * 0.5;
  d[1] := (nH - H) * 0.5;
  R[0] := Vec2Sub(R[0], d);
  R[1] := Vec2Add(R[1], d);

  R := FixRect(R);
  R[0, 0] := Max(R[0, 0], 0);
  R[0, 1] := Max(R[0, 1], 0);
  R[1, 0] := Min(R[1, 0], Width);
  R[1, 1] := Min(R[1, 1], Height);

  Result := R;
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

function TMemoryRaster.BuildAreaCopyAs(clipArea: TRectV2): TMemoryRaster;
var
  R: TRectV2;
  W, H: TGeoFloat;
begin
  R := FixRect(clipArea);
  W := RectWidth(R);
  H := RectHeight(R);

  Result := NewRaster();
  Result.SetSize(Round(W), Round(H));
  ProjectionTo(Result, TV2Rect4.Init(R, 0), TV2Rect4.Init(Result.BoundsRectV2, 0), True, 1.0);
end;

function TMemoryRaster.BuildAreaCopyAs(clipArea: TRect): TMemoryRaster;
begin
  Result := BuildAreaCopyAs(RectV2(clipArea));
end;

function TMemoryRaster.FastAreaCopyAs(X1, Y1, X2, Y2: TGeoInt): TMemoryRaster;
var
  W, H, j, i: Integer;
begin
  ForwardRect(X1, Y1, X2, Y2);
  W := X2 - X1;
  H := Y2 - Y1;
  Result := NewRaster();
  Result.SetSize(W, H);
  ReadyBits;
  for j := 0 to W - 1 do
    for i := 0 to H - 1 do
      if InHere(i + X1, j + Y1) then
          Result.FastPixel[i, j] := FastPixel[i + X1, j + Y1]
      else
          Result.FastPixel[i, j] := RColor(0, 0, 0);
end;

procedure TMemoryRaster.FastAreaCopyFrom(Source: TMemoryRaster; DestX, DestY: Integer);
var
  i, j: Integer;
begin
  for j := 0 to Source.Height - 1 do
    for i := 0 to Source.Width - 1 do
      if InHere(i + DestX, j + DestY) then
          FastPixel[i + DestX, j + DestY] := Source.FastPixel[i, j];
end;

function TMemoryRaster.ExistsColor(C: TRColor): Boolean;
var
  i, j: Integer;
begin
  Result := True;
  for j := 0 to Height - 1 do
    for i := 0 to Width - 1 do
      if Pixel[i, j] = C then
          exit;
  Result := False;
end;

function TMemoryRaster.FindFirstColor(C: TRColor): TPoint;
var
  i, j: Integer;
begin
  Result := Point(-1, -1);
  for j := 0 to Height - 1 do
    for i := 0 to Width - 1 do
      if Pixel[i, j] = C then
        begin
          Result := Point(i, j);
          exit;
        end;
end;

function TMemoryRaster.FindLastColor(C: TRColor): TPoint;
var
  i, j: Integer;
begin
  Result := Point(-1, -1);
  for j := Height - 1 downto 0 do
    for i := Width - 1 downto 0 do
      if Pixel[i, j] = C then
        begin
          Result := Point(i, j);
          exit;
        end;
end;

function TMemoryRaster.FindNearColor(C: TRColor; Pt: TVec2): TPoint;
var
  i, j: Integer;
  npt: TVec2;
  d1, d2: TGeoFloat;
begin
  Result := Point(-1, -1);
  npt := Vec2(-1, -1);
  d2 := Vec2Distance(npt, Pt);
  d1 := d2;

  for j := 0 to Height - 1 do
    for i := 0 to Width - 1 do
      if Pixel[i, j] = C then
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

function TMemoryRaster.ColorBoundsRectV2(C: TRColor): TRectV2;
var
  color_inited_: Boolean;
  i, j: Integer;
begin
  color_inited_ := False;
  Result := NullRectV2;
  for j := 0 to Height - 1 do
    for i := 0 to Width - 1 do
      if Pixel[i, j] = C then
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

function TMemoryRaster.ColorBoundsRect(C: TRColor): TRect;
var
  color_inited_: Boolean;
  i, j: Integer;
begin
  color_inited_ := False;
  Result := Rect(0, 0, 0, 0);
  for j := 0 to Height - 1 do
    for i := 0 to Width - 1 do
      if Pixel[i, j] = C then
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

function TMemoryRaster.ConvexHull(C: TRColor): TVec2List;
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
            if Pixel[i, j] <> C then
              begin
                Result.Add(i, j);
                found := False;
              end;
          end
        else if Pixel[i, j] = C then
          begin
            Result.Add(i, j);
            found := True;
          end;
    end;
end;

function TMemoryRaster.NoneColorBoundsRectV2(C: TRColor): TRectV2;
var
  color_inited_: Boolean;
  i, j: Integer;
begin
  color_inited_ := False;
  Result := NullRectV2;
  for j := 0 to Height - 1 do
    for i := 0 to Width - 1 do
      if Pixel[i, j] <> C then
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

function TMemoryRaster.NoneColorBoundsRect(C: TRColor): TRect;
var
  color_inited_: Boolean;
  i, j: Integer;
begin
  color_inited_ := False;
  Result := Rect(0, 0, 0, 0);
  for j := 0 to Height - 1 do
    for i := 0 to Width - 1 do
      if Pixel[i, j] <> C then
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

procedure TMemoryRaster.BlendColor(bk: TRColor);
var
  S: TRColorEntry;
  p: PRColorEntry;
  i: Integer;
begin
  S.BGRA := bk;
  ReadyBits();
  p := @FBits^[0];
  for i := 0 to Width * Height - 1 do
    begin
      S.A := $FF - p^.A;
      BlendMem(S.BGRA, p^.BGRA);
      p^.A := $FF - S.A;
      inc(p);
    end;
end;

procedure TMemoryRaster.BlendBlack();
begin
  BlendColor(RColor(0, 0, 0));
end;

procedure TMemoryRaster.Black();
begin
  BlendColor(RColor(0, 0, 0));
end;

procedure TMemoryRaster.Line(X1, Y1, X2, Y2: Integer; Color: TRColor; L: Boolean);
  procedure VertLine(X, Y1, Y2: Integer);
  var
    i, nH, NL: Integer;
    p: PRColor;
  begin
    if (X < 0) or (X >= Width) then
        exit;
    Y1 := ClampInt(Y1, 0, Height);
    Y2 := ClampInt(Y2, 0, Height);

    if Y2 < Y1 then
        CoreClasses.Swap(Y1, Y2);

    p := PixelPtr[X, Y1];
    i := Y2 - Y1 + 1;
    nH := i shr 2;
    NL := i and $03;
    for i := 0 to nH - 1 do
      begin
        BlendMem(Color, p^);
        inc(p, Width);
        BlendMem(Color, p^);
        inc(p, Width);
        BlendMem(Color, p^);
        inc(p, Width);
        BlendMem(Color, p^);
        inc(p, Width);
      end;
    for i := 0 to NL - 1 do
      begin
        BlendMem(Color, p^);
        inc(p, Width);
      end;
  end;

  procedure HorzLine(X1, Y, X2: Integer);
  var
    i: Integer;
    p: PRColor;
  begin
    if (Y < 0) or (Y >= Height) then
        exit;
    X1 := ClampInt(X1, 0, Width - 1);
    X2 := ClampInt(X2, 0, Width - 1);

    if X1 > X2 then
        CoreClasses.Swap(X1, X2);

    p := PixelPtr[X1, Y];

    for i := X1 to X2 do
      begin
        BlendMem(Color, p^);
        inc(p);
      end;
  end;

var
  dy, dx, SY, SX, i, delta: Integer;
  pi, pl: Integer;
begin
  ReadyBits();

  if (X1 = X2) and (Y1 = Y2) then
    begin
      Pixel[X1, Y1] := Color;
      exit;
    end;

  if FAgg <> nil then
    begin
      FAgg.LineColor := Color;
      FAgg.Line(X1, Y1, X2, Y2);
      exit;
    end;

  dx := X2 - X1;
  dy := Y2 - Y1;

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
          VertLine(X1, Y1, Y2 - 1)
      else if dy < 0 then
          VertLine(X1, Y2 + 1, Y1);
      if L then
          Pixel[X2, Y2] := Color;
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
      if X2 > X1 then
          HorzLine(X1, Y1, X2 - 1)
      else
          HorzLine(X2 + 1, Y1, X1);
      if L then
          Pixel[X2, Y2] := Color;
      exit;
    end;

  pi := X1 + Y1 * Width;
  SY := SY * Width;
  pl := Width * Height;

  if dx > dy then
    begin
      delta := dx shr 1;
      for i := 0 to dx - 1 do
        begin
          if (pi >= 0) and (pi < pl) then
              BlendMem(Color, FBits^[pi]);

          inc(pi, SX);
          inc(delta, dy);
          if delta >= dx then
            begin
              inc(pi, SY);
              dec(delta, dx);
            end;
        end;
    end
  else // Dx < Dy
    begin
      delta := dy shr 1;
      for i := 0 to dy - 1 do
        begin
          if (pi >= 0) and (pi < pl) then
              BlendMem(Color, FBits^[pi]);

          inc(pi, SY);
          inc(delta, dx);
          if delta >= dy then
            begin
              inc(pi, SX);
              dec(delta, dy);
            end;
        end;
    end;
  if (L) and (pi >= 0) and (pi < pl) then
      BlendMem(Color, FBits^[pi]);
end;

procedure TMemoryRaster.LineF(X1, Y1, X2, Y2: TGeoFloat; Color: TRColor; L: Boolean);
begin
  Line(Round(X1), Round(Y1), Round(X2), Round(Y2), Color, L);
end;

procedure TMemoryRaster.LineF(p1, p2: TVec2; Color: TRColor; L: Boolean);
begin
  LineF(p1[0], p1[1], p2[0], p2[1], Color, L);
end;

procedure TMemoryRaster.LineF(p1, p2: TVec2; Color: TRColor; L: Boolean; LineDist: TGeoFloat; Cross: Boolean);
begin
  LineF(p1[0], p1[1], p2[0], p2[1], Color, L);
  if not Cross then
      exit;
  DrawCrossF(p1, LineDist, Color);
  DrawCrossF(p2, LineDist, Color);
end;

procedure TMemoryRaster.FillRect(X1, Y1, X2, Y2: Integer; Color: TRColor);
var
  j, i: Integer;
  p: PRColor;
begin
  if FAgg <> nil then
    begin
      FAgg.FillColor := Color;
      FAgg.NoLine;
      FAgg.Rectangle(X1, Y1, X2, Y2);
    end
  else
    begin
      FixRect(X1, Y1, X2, Y2);

      if X1 < 0 then
          X1 := 0
      else if X1 >= Width then
          X1 := Width - 1;

      if X2 < 0 then
          X2 := 0
      else if X2 >= Width then
          X2 := Width - 1;

      if Y1 < 0 then
          Y1 := 0
      else if Y1 >= Height then
          Y1 := Height - 1;
      if Y2 < 0 then
          Y2 := 0
      else if Y2 >= Height then
          Y2 := Height - 1;

      for j := Y1 to Y2 - 1 do
        begin
          i := X1;
          p := @ScanLine[j]^[i];
          while i < X2 do
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
  l2, X1, Y1, X2, Y2: Integer;
begin
  l2 := LineDist div 2;
  X1 := Dstx - l2;
  Y1 := Dsty - l2;
  X2 := Dstx + l2;
  Y2 := Dsty + l2;
  FillRect(X1, Y1, X2, Y2, Color);
end;

procedure TMemoryRaster.FillRect(Dst: TVec2; LineDist: Integer; Color: TRColor);
begin
  FillRect(Round(Dst[0]), Round(Dst[1]), LineDist, Color);
end;

procedure TMemoryRaster.FillRect(R: TRect; Color: TRColor);
begin
  FillRect(R.Left, R.Top, R.Right, R.Bottom, Color);
end;

procedure TMemoryRaster.FillRect(R: TRectV2; Color: TRColor);
begin
  FillRect(Round(R[0, 0]), Round(R[0, 1]), Round(R[1, 0]), Round(R[1, 1]), Color);
end;

procedure TMemoryRaster.FillRect(R: TRectV2; Angle: TGeoFloat; Color: TRColor);
var
  A: TGeoFloat;
  r4: TV2Rect4;
  buff: array [0 .. 4] of TPointDouble;
begin
  A := NormalizeDegAngle(Angle);
  if A = 0 then
      FillRect(R, Color)
  else
    begin
      r4 := TV2Rect4.Init(R, A);
      if FAgg <> nil then
        begin
          buff[0].X := r4.LeftTop[0];
          buff[0].Y := r4.LeftTop[1];
          buff[1].X := r4.RightTop[0];
          buff[1].Y := r4.RightTop[1];
          buff[2].X := r4.RightBottom[0];
          buff[2].Y := r4.RightBottom[1];
          buff[3].X := r4.LeftBottom[0];
          buff[3].Y := r4.LeftBottom[1];
          buff[4].X := r4.LeftTop[0];
          buff[4].Y := r4.LeftTop[1];

          FAgg.FillColor := Color;
          FAgg.NoLine;
          FAgg.Polygon(@buff[0], 5);
        end
      else
          Projection(r4, Color);
    end;
end;

procedure TMemoryRaster.DrawRect(R: TRect; Color: TRColor);
begin
  DrawRect(RectV2(R), Color);
end;

procedure TMemoryRaster.DrawRect(R: TRectV2; Color: TRColor);
begin
  if FAgg <> nil then
    begin
      FAgg.NoFill;
      FAgg.LineColor := Color;
      FAgg.Rectangle(R[0][0], R[0][1], R[1][0], R[1][1]);
    end
  else
    begin
      LineF(Vec2(R[0][0], R[0][1]), Vec2(R[1][0], R[0][1]), Color, True);
      LineF(Vec2(R[1][0], R[0][1]), Vec2(R[1][0], R[1][1]), Color, True);
      LineF(Vec2(R[1][0], R[1][1]), Vec2(R[0][0], R[1][1]), Color, True);
      LineF(Vec2(R[0][0], R[1][1]), Vec2(R[0][0], R[0][1]), Color, True);
    end;
end;

procedure TMemoryRaster.DrawRect(R: TV2Rect4; Color: TRColor);
var
  buff: array [0 .. 4] of TPointDouble;
begin
  if R.BoundArea < 1 then
      exit;
  if FAgg <> nil then
    begin
      buff[0].X := R.LeftTop[0];
      buff[0].Y := R.LeftTop[1];
      buff[1].X := R.RightTop[0];
      buff[1].Y := R.RightTop[1];
      buff[2].X := R.RightBottom[0];
      buff[2].Y := R.RightBottom[1];
      buff[3].X := R.LeftBottom[0];
      buff[3].Y := R.LeftBottom[1];
      buff[4].X := R.LeftTop[0];
      buff[4].Y := R.LeftTop[1];
      FAgg.NoFill;
      FAgg.LineColor := Color;
      FAgg.Polyline(@buff[0], 5);
    end
  else
    begin
      LineF(R.LeftTop, R.RightTop, Color, True);
      LineF(R.RightTop, R.RightBottom, Color, True);
      LineF(R.RightBottom, R.LeftBottom, Color, True);
      LineF(R.LeftBottom, R.LeftTop, Color, True);
    end;
end;

procedure TMemoryRaster.DrawRect(R: TRectV2; Angle: TGeoFloat; Color: TRColor);
var
  r4: TV2Rect4;
  buff: array [0 .. 4] of TPointDouble;
begin
  r4 := TV2Rect4.Init(R, Angle);
  if FAgg <> nil then
    begin
      buff[0].X := r4.LeftTop[0];
      buff[0].Y := r4.LeftTop[1];
      buff[1].X := r4.RightTop[0];
      buff[1].Y := r4.RightTop[1];
      buff[2].X := r4.RightBottom[0];
      buff[2].Y := r4.RightBottom[1];
      buff[3].X := r4.LeftBottom[0];
      buff[3].Y := r4.LeftBottom[1];
      buff[4].X := r4.LeftTop[0];
      buff[4].Y := r4.LeftTop[1];

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
      LineF(Vec2Mul(tri[0], Size0), Vec2Mul(tri[1], Size0), Color, True, 5, Cross);
      LineF(Vec2Mul(tri[1], Size0), Vec2Mul(tri[2], Size0), Color, True, 5, Cross);
      LineF(Vec2Mul(tri[2], Size0), Vec2Mul(tri[0], Size0), Color, True, 5, Cross);
    end
  else
    begin
      LineF(tri[0], tri[1], Color, True, 5, Cross);
      LineF(tri[1], tri[2], Color, True, 5, Cross);
      LineF(tri[2], tri[0], Color, True, 5, Cross);
    end;
end;

procedure TMemoryRaster.DrawFlatCross(Dst: TVec2; LineDist: TGeoFloat; Color: TRColor);
var
  L, X1, Y1, X2, Y2: TGeoFloat;
begin
  L := LineDist * 0.5;

  X1 := Dst[0] - L;
  X2 := Dst[0] + L;
  Y1 := Dst[1];
  Y2 := Dst[1];
  LineF(X1, Y1, X2, Y2, Color, True);

  X1 := Dst[0];
  X2 := Dst[0];
  Y1 := Dst[1] - L;
  Y2 := Dst[1] + L;
  LineF(X1, Y1, X2, Y2, Color, True);
end;

procedure TMemoryRaster.DrawCross(Dstx, Dsty, LineDist: Integer; Color: TRColor);
var
  L, X1, Y1, X2, Y2: Integer;
begin
  L := LineDist div 2;

  X1 := Dstx - L;
  Y1 := Dsty - L;
  X2 := Dstx + L;
  Y2 := Dsty + L;
  Line(X1, Y1, X2, Y2, Color, True);

  X1 := Dstx - L;
  Y1 := Dsty + L;
  X2 := Dstx + L;
  Y2 := Dsty - L;
  Line(X1, Y1, X2, Y2, Color, True);
end;

procedure TMemoryRaster.DrawCrossF(Dstx, Dsty, LineDist: TGeoFloat; Color: TRColor);
begin
  DrawCross(Round(Dstx), Round(Dsty), Round(LineDist), Color);
end;

procedure TMemoryRaster.DrawCrossF(Dst: TVec2; LineDist: TGeoFloat; Color: TRColor);
begin
  DrawCrossF(Dst[0], Dst[1], LineDist, Color);
end;

procedure TMemoryRaster.DrawCrossF(Polygon: TVec2List; LineDist: TGeoFloat; Color: TRColor);
var
  i: Integer;
begin
  for i := 0 to Polygon.Count - 1 do
      DrawCrossF(Polygon[i]^, LineDist, Color);
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
          buff[i].X := p1^[0];
          buff[i].Y := p1^[1];
        end;
      if wasClose then
        begin
          p1 := pl.First;
          buff[pl.Count].X := p1^[0];
          buff[pl.Count].Y := p1^[1];
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

procedure TMemoryRaster.DrawCircle(CC: TVec2; R: TGeoFloat; Color: TRColor);
var
  vl: TVec2List;
begin
  if FAgg <> nil then
    begin
      FAgg.NoFill;
      FAgg.LineColor := Color;
      FAgg.Circle(CC[0], CC[1], R);
    end
  else
    begin
      vl := TVec2List.Create;
      vl.AddCirclePoint(Round(R), CC, R);
      DrawPointListLine(vl, Color, True);
      DisposeObject(vl);
    end;
end;

procedure TMemoryRaster.FillCircle(CC: TVec2; R: TGeoFloat; Color: TRColor);
var
  vl: TVec2List;
begin
  if FAgg <> nil then
    begin
      FAgg.FillColor := Color;
      FAgg.NoLine;
      FAgg.Circle(CC[0], CC[1], R);
    end
  else
    begin
      vl := TVec2List.Create;
      vl.AddCirclePoint(Round(R), CC, R);
      Vertex.FillPoly(vl, CC, Color);
      DisposeObject(vl);
    end;
end;

procedure TMemoryRaster.DrawEllipse(CC: TVec2; xRadius, yRadius: TGeoFloat; Color: TRColor);
var
  i, n: Integer;
  S, C: TGeoFloatArray;
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
      SetLength(S, n);
      SetLength(C, n);
      dec(n);
      BuildSinCosCache(@S, @C, 0, 90);
      for i := 0 to n do
        begin
          S[i] := S[i] * yRadius;
          C[i] := C[i] * xRadius;
        end;
      vl := TVec2List.Create;
      // first quadrant (top right)
      for i := 0 to n do
          vl.Add(CC[0] + C[i], CC[1] - S[i]);
      // second quadrant (top left)
      for i := n - 1 downto 0 do
          vl.Add(CC[0] - C[i], CC[1] - S[i]);
      // third quadrant (bottom left)
      for i := 1 to n do
          vl.Add(CC[0] - C[i], CC[1] + S[i]);
      // fourth quadrant (bottom right)
      for i := n - 1 downto 0 do
          vl.Add(CC[0] + C[i], CC[1] + S[i]);
      SetLength(S, 0);
      SetLength(C, 0);
      DrawPointListLine(vl, Color, False);
      DisposeObject(vl);
    end;
end;

procedure TMemoryRaster.DrawEllipse(R: TRectV2; Color: TRColor);
var
  cen: TVec2;
begin
  cen := RectCentre(R);
  DrawEllipse(cen, abs(R[0][0] - cen[0]), abs(R[1][1] - cen[1]), Color);
end;

procedure TMemoryRaster.FillEllipse(CC: TVec2; xRadius, yRadius: TGeoFloat; Color: TRColor);
var
  i, n: Integer;
  S, C: TGeoFloatArray;
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
      SetLength(S, n);
      SetLength(C, n);
      dec(n);
      BuildSinCosCache(@S, @C, 0, 90);
      for i := 0 to n do
        begin
          S[i] := S[i] * yRadius;
          C[i] := C[i] * xRadius;
        end;
      vl := TVec2List.Create;
      // first quadrant (top right)
      for i := 0 to n do
          vl.Add(CC[0] + C[i], CC[1] - S[i]);
      // second quadrant (top left)
      for i := n - 1 downto 0 do
          vl.Add(CC[0] - C[i], CC[1] - S[i]);
      // third quadrant (bottom left)
      for i := 1 to n do
          vl.Add(CC[0] - C[i], CC[1] + S[i]);
      // fourth quadrant (bottom right)
      for i := n - 1 downto 0 do
          vl.Add(CC[0] + C[i], CC[1] + S[i]);
      SetLength(S, 0);
      SetLength(C, 0);
      Vertex.FillPoly(vl, CC, Color);
      DisposeObject(vl);
    end;
end;

procedure TMemoryRaster.FillEllipse(R: TRectV2; Color: TRColor);
var
  cen: TVec2;
begin
  cen := RectCentre(R);
  DrawEllipse(cen, abs(R[0][0] - cen[0]), abs(R[1][1] - cen[1]), Color);
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
        buff[i].X := p1^[0];
        buff[i].Y := p1^[1];
      end;

    p1 := @PolygonBuff[0];
    buff[L].X := p1^[0];
    buff[L].Y := p1^[1];

    FAgg.NoLine;
    FAgg.FillColor := Color;
    FAgg.Polygon(@buff[0], L + 1, dpfFillOnly);
    SetLength(buff, 0);
  end;

  procedure Fill_Geo;
  var
    R: TRectV2;
{$IFDEF Parallel}
{$IFDEF FPC}
    procedure Nested_ParallelFor(pass: Integer);
    var
      p: PRColorArray;
      i: Integer;
      Pt: TVec2;
    begin
      p := ScanLine[pass];
      for i := 0 to Width - 1 do
        begin
          Pt := Vec2(i, pass);
          if PointInRect(Pt, R) and PointInPolygon(Pt, PolygonBuff) then
              p^[i] := Color;
        end;
    end;
{$ENDIF FPC}
{$ELSE Parallel}
    procedure DoFor;
    var
      p: PRColorArray;
      pass, i: Integer;
      Pt: TVec2;
    begin
      for pass := 0 to Height - 1 do
        begin
          p := ScanLine[pass];
          for i := 0 to Width - 1 do
            begin
              Pt := Vec2(i, pass);
              if PointInRect(Pt, R) and PointInPolygon(Pt, PolygonBuff) then
                  p^[i] := Color;
            end;
        end;
    end;
{$ENDIF Parallel}

  begin
    R := BoundRect(PolygonBuff);
{$IFDEF Parallel}
{$IFDEF FPC}
    FPCParallelFor(TMemoryRaster.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE}
    DelphiParallelFor(TMemoryRaster.Parallel and LocalParallel, 0, Height - 1, procedure(pass: Integer)
      var
        p: PRColorArray;
        i: Integer;
        Pt: TVec2;
      begin
        p := ScanLine[pass];
        for i := 0 to Width - 1 do
          begin
            Pt := Vec2(i, pass);
            if PointInRect(Pt, R) and PointInPolygon(Pt, PolygonBuff) then
                p^[i] := Color;
          end;
      end);
{$ENDIF FPC}
{$ELSE}
    DoFor();
{$ENDIF Parallel}
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
          buff[i].X := p1^[0];
          buff[i].Y := p1^[1];
        end;

      p1 := @PolygonBuff[0];
      buff[L].X := p1^[0];
      buff[L].Y := p1^[1];

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
        buff[i].X := p1^[0];
        buff[i].Y := p1^[1];
      end;

    p1 := Polygon[0];
    buff[L].X := p1^[0];
    buff[L].Y := p1^[1];

    FAgg.NoLine;
    FAgg.FillColor := Color;
    FAgg.Polygon(@buff[0], L + 1, dpfFillOnly);
    SetLength(buff, 0);
  end;

  procedure Fill_Geo;
  var
    R: TRectV2;
{$IFDEF Parallel}
{$IFDEF FPC}
    procedure Nested_ParallelFor(pass: Integer);
    var
      p: PRColorArray;
      i: Integer;
      Pt: TVec2;
    begin
      p := ScanLine[pass];
      for i := 0 to Width - 1 do
        begin
          Pt := Vec2(i, pass);
          if PointInRect(Pt, R) and Polygon.InHere(Pt) then
              p^[i] := Color;
        end;
    end;
{$ENDIF FPC}
{$ELSE Parallel}
    procedure DoFor;
    var
      p: PRColorArray;
      pass, i: Integer;
      Pt: TVec2;
    begin
      for pass := 0 to Height - 1 do
        begin
          p := ScanLine[pass];
          for i := 0 to Width - 1 do
            begin
              Pt := Vec2(i, pass);
              if PointInRect(Pt, R) and Polygon.InHere(Pt) then
                  p^[i] := Color;
            end;
        end;
    end;
{$ENDIF Parallel}

  begin
    R := Polygon.BoundBox();
{$IFDEF Parallel}
{$IFDEF FPC}
    FPCParallelFor(TMemoryRaster.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE}
    DelphiParallelFor(TMemoryRaster.Parallel and LocalParallel, 0, Height - 1, procedure(pass: Integer)
      var
        p: PRColorArray;
        i: Integer;
        Pt: TVec2;
      begin
        p := ScanLine[pass];
        for i := 0 to Width - 1 do
          begin
            Pt := Vec2(i, pass);
            if PointInRect(Pt, R) and Polygon.InHere(Pt) then
                p^[i] := Color;
          end;
      end);
{$ENDIF FPC}
{$ELSE}
    DoFor();
{$ENDIF Parallel}
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
          buff[i].X := p1^[0];
          buff[i].Y := p1^[1];
        end;

      p1 := Polygon[0];
      buff[L].X := p1^[0];
      buff[L].Y := p1^[1];

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
    R: TRectV2;
{$IFDEF Parallel}
{$IFDEF FPC}
    procedure Nested_ParallelFor(pass: Integer);
    var
      p: PRColorArray;
      i: Integer;
      Pt: TVec2;
    begin
      p := ScanLine[pass];
      for i := 0 to Width - 1 do
        begin
          Pt := Vec2(i, pass);
          if PointInRect(Pt, R) and Polygon.InHere(Pt) then
              p^[i] := Color;
        end;
    end;
{$ENDIF FPC}
{$ELSE Parallel}
    procedure DoFor;
    var
      p: PRColorArray;
      pass, i: Integer;
      Pt: TVec2;
    begin
      for pass := 0 to Height - 1 do
        begin
          p := ScanLine[pass];
          for i := 0 to Width - 1 do
            begin
              Pt := Vec2(i, pass);
              if PointInRect(Pt, R) and Polygon.InHere(Pt) then
                  p^[i] := Color;
            end;
        end;
    end;
{$ENDIF Parallel}

  begin
    R := Polygon.BoundBox();
{$IFDEF Parallel}
{$IFDEF FPC}
    FPCParallelFor(TMemoryRaster.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE}
    DelphiParallelFor(TMemoryRaster.Parallel and LocalParallel, 0, Height - 1, procedure(pass: Integer)
      var
        p: PRColorArray;
        i: Integer;
        Pt: TVec2;
      begin
        p := ScanLine[pass];
        for i := 0 to Width - 1 do
          begin
            Pt := Vec2(i, pass);
            if PointInRect(Pt, R) and Polygon.InHere(Pt) then
                p^[i] := Color;
          end;
      end);
{$ENDIF FPC}
{$ELSE}
    DoFor();
{$ENDIF Parallel}
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

procedure TMemoryRaster.DrawPolygon(Polygon: T2DPolygonGraph; SurroundColor, CollapseColor: TRColor);
var
  i: Integer;
begin
  DrawPolygon(Polygon.Surround, SurroundColor);
  for i := 0 to Polygon.CollapsesCount - 1 do
      DrawPolygon(Polygon.Bands[i], CollapseColor);
end;

procedure TMemoryRaster.DrawPolygonCross(Polygon: T2DPolygonGraph; LineDist: TGeoFloat; SurroundColor, CollapseColor: TRColor);
var
  i: Integer;
begin
  DrawCrossF(Polygon.Surround, LineDist, SurroundColor);
  for i := 0 to Polygon.CollapsesCount - 1 do
      DrawCrossF(Polygon.Bands[i], LineDist, CollapseColor);
end;

procedure TMemoryRaster.DrawPolygonLine(Polygon: TLines; Color: TRColor; wasClose: Boolean);
var
  L, i: Integer;
  p1, p2: PVec2;
begin
  L := Polygon.Count;
  if L < 2 then
      exit;

  for i := 1 to L - 1 do
    begin
      p1 := Polygon[i - 1];
      p2 := Polygon[i];
      LineF(p1^, p2^, Color, True);
    end;

  if wasClose then
    begin
      p1 := Polygon[0];
      p2 := Polygon[L - 1];
      LineF(p1^, p2^, Color, True);
    end;
end;

procedure TMemoryRaster.DrawPolygon(Polygon: TDeflectionPolygon; ExpandDist: TGeoFloat; Color: TRColor);
var
  L, i: Integer;
  p1, p2: TVec2;
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
          p1 := Polygon.Expands[i, ExpandDist];
          buff[i].X := p1[0];
          buff[i].Y := p1[1];
        end;

      p1 := Polygon.Expands[0, ExpandDist];
      buff[L].X := p1[0];
      buff[L].Y := p1[1];

      FAgg.NoFill;
      FAgg.LineColor := Color;
      FAgg.Polygon(@buff[0], L + 1, dpfStrokeOnly);
      SetLength(buff, 0);
    end
  else
    begin
      for i := 1 to L - 1 do
        begin
          p1 := Polygon.Expands[i - 1, ExpandDist];
          p2 := Polygon.Expands[i, ExpandDist];
          LineF(p1, p2, Color, True);
        end;

      p1 := Polygon.Expands[0, ExpandDist];
      p2 := Polygon.Expands[L - 1, ExpandDist];
      LineF(p1, p2, Color, True);
    end;
end;

function TMemoryRaster.PixelAtNoneBGBorder(const X, Y: Integer; const BGColor, BorderColor: TRColor; const halfBorderSize: Integer; var detectColor: TRColor): Boolean;
var
  i, j: Integer;
  C: TRColor;
begin
  if Pixel[X, Y] = BGColor then
    for j := Y - halfBorderSize to Y + halfBorderSize do
      if (j >= 0) and (j < Height) then
        for i := X - halfBorderSize to X + halfBorderSize do
          if (i >= 0) and (i < Width) then
            begin
              C := Pixel[i, j];
              if (C <> BGColor) and (C <> BorderColor) then
                begin
                  Result := True;
                  detectColor := C;
                  exit;
                end;
            end;
  Result := False;
end;

function TMemoryRaster.PixelAtNoneBGBorder(const X, Y: Integer; const BGColor: TRColor; const halfBorderSize: Integer; var detectColor: TRColor): Boolean;
var
  i, j: Integer;
  C: TRColor;
begin
  if Pixel[X, Y] = BGColor then
    for j := Y - halfBorderSize to Y + halfBorderSize do
      if (j >= 0) and (j < Height) then
        for i := X - halfBorderSize to X + halfBorderSize do
          if (i >= 0) and (i < Width) then
            begin
              C := Pixel[i, j];
              if (C <> BGColor) then
                begin
                  Result := True;
                  detectColor := C;
                  exit;
                end;
            end;
  Result := False;
end;

procedure TMemoryRaster.FillNoneBGColorBorder(parallel_: Boolean; BGColor, BorderColor: TRColor; BorderSize: Integer);
var
  halfBorderSize: Integer;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  var
    i: Integer;
    detectColor: TRColor;
  begin
    for i := 0 to Width - 1 do
      if PixelAtNoneBGBorder(i, pass, BGColor, BorderColor, halfBorderSize, detectColor) then
          Pixel[i, pass] := BorderColor;
  end;
{$ENDIF FPC}
{$ENDIF Parallel}
  procedure DoFor;
  var
    i, pass: Integer;
    detectColor: TRColor;
  begin
    for pass := 0 to Height - 1 do
      for i := 0 to Width - 1 do
        if PixelAtNoneBGBorder(i, pass, BGColor, BorderColor, halfBorderSize, detectColor) then
            Pixel[i, pass] := BorderColor;
  end;

begin
  if BGColor = BorderColor then
      exit;
  halfBorderSize := BorderSize shr 1;
  if halfBorderSize <= 0 then
      halfBorderSize := 1;

  if parallel_ then
    begin
{$IFDEF Parallel}
{$IFDEF FPC}
      FPCParallelFor(TMemoryRaster.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
      DelphiParallelFor(TMemoryRaster.Parallel and LocalParallel, 0, Height - 1, procedure(pass: Integer)
        var
          i: Integer;
          detectColor: TRColor;
        begin
          for i := 0 to Width - 1 do
            if PixelAtNoneBGBorder(i, pass, BGColor, BorderColor, halfBorderSize, detectColor) then
                Pixel[i, pass] := BorderColor;
        end);
{$ENDIF FPC}
{$ELSE Parallel}
      DoFor;
{$ENDIF Parallel}
    end
  else
      DoFor;
end;

procedure TMemoryRaster.FillNoneBGColorBorder(BGColor, BorderColor: TRColor; BorderSize: Integer);
begin
  FillNoneBGColorBorder(True, BGColor, BorderColor, BorderSize);
end;

procedure TMemoryRaster.FillNoneBGColorAlphaBorder(parallel_: Boolean; BGColor, BorderColor: TRColor; BorderSize: Integer; output: TMemoryRaster);
var
  halfBorderSize: Integer;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  var
    i: Integer;
    detectColor: TRColor;
  begin
    for i := 0 to Width - 1 do
      if PixelAtNoneBGBorder(i, pass, BGColor, BorderColor, halfBorderSize, detectColor) then
          output[i, pass] := RAlphaColor(BorderColor, PixelAlpha[i, pass]);
  end;
{$ENDIF FPC}
{$ENDIF Parallel}
  procedure DoFor;
  var
    i, pass: Integer;
    detectColor: TRColor;
  begin
    for pass := 0 to Height - 1 do
      for i := 0 to Width - 1 do
        if PixelAtNoneBGBorder(i, pass, BGColor, BorderColor, halfBorderSize, detectColor) then
            output[i, pass] := RAlphaColor(BorderColor, PixelAlpha[i, pass]);
  end;

begin
  halfBorderSize := BorderSize shr 1;
  if halfBorderSize <= 0 then
      halfBorderSize := 1;

  if parallel_ then
    begin
{$IFDEF Parallel}
{$IFDEF FPC}
      FPCParallelFor(TMemoryRaster.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
      DelphiParallelFor(TMemoryRaster.Parallel and LocalParallel, 0, Height - 1, procedure(pass: Integer)
        var
          i: Integer;
          detectColor: TRColor;
        begin
          for i := 0 to Width - 1 do
            if PixelAtNoneBGBorder(i, pass, BGColor, BorderColor, halfBorderSize, detectColor) then
                output[i, pass] := RAlphaColor(BorderColor, PixelAlpha[i, pass]);
        end);
{$ENDIF FPC}
{$ELSE Parallel}
      DoFor;
{$ENDIF Parallel}
    end
  else
      DoFor;
end;

procedure TMemoryRaster.FillNoneBGColorAlphaBorder(BGColor, BorderColor: TRColor; BorderSize: Integer; output: TMemoryRaster);
begin
  FillNoneBGColorAlphaBorder(True, BGColor, BorderColor, BorderSize, output);
end;

function TMemoryRaster.TextSize(Text: SystemString; siz: TGeoFloat): TVec2;
begin
  Result := Vec2Mul(Font.TextSize(Text), siz / Font.FontSize)
end;

procedure TMemoryRaster.DrawText(Text: SystemString; X, Y: TGeoFloat; RotateVec: TVec2; Angle, alpha, siz: TGeoFloat; TextColor: TRColor);
begin
  Font.DrawText(Text, Self, X, Y, RotateVec, Angle, alpha, siz, TextColor);
end;

procedure TMemoryRaster.DrawText(Text: SystemString; X, Y: TGeoFloat; siz: TGeoFloat; TextColor: TRColor);
begin
  DrawText(Text, X, Y, Vec2(0.5, 0.5), 0, 1, siz, TextColor);
end;

procedure TMemoryRaster.DrawText(Text: SystemString; X, Y: TGeoFloat; RotateVec: TVec2; Angle, alpha, siz: TGeoFloat; TextColor: TRColor; var DrawCoordinate: TArrayV2R4);
begin
  Font.DrawText(Text, Self, X, Y, RotateVec, Angle, alpha, siz, TextColor, DrawCoordinate);
end;

procedure TMemoryRaster.DrawText(Text: SystemString; X, Y: TGeoFloat; siz: TGeoFloat; TextColor: TRColor; var DrawCoordinate: TArrayV2R4);
begin
  DrawText(Text, X, Y, Vec2(0.5, 0.5), 0, 1, siz, TextColor, DrawCoordinate);
end;

function TMemoryRaster.ComputeDrawTextCoordinate(Text: SystemString; X, Y: TGeoFloat; RotateVec: TVec2; Angle, siz: TGeoFloat; var DrawCoordinate, BoundBoxCoordinate: TArrayV2R4): TVec2;
begin
  Result := Font.ComputeDrawCoordinate(Text, X, Y, RotateVec, Angle, siz, DrawCoordinate, BoundBoxCoordinate);
end;

function TMemoryRaster.ComputeTextSize(Text: SystemString; RotateVec: TVec2; Angle, siz: TGeoFloat): TVec2;
begin
  Result := Font.ComputeTextSize(Text, RotateVec, Angle, siz);
end;

function TMemoryRaster.ComputeTextConvexHull(Text: SystemString; X, Y: TGeoFloat; RotateVec: TVec2; Angle, siz: TGeoFloat): TArrayVec2;
begin
  Result := Font.ComputeTextConvexHull(Text, X, Y, RotateVec, Angle, siz);
end;

function TMemoryRaster.GetDrawEngineMap: TCoreClassObject;
var
  d: TDrawEngine;
begin
  if FDrawEngineMap = nil then
      FDrawEngineMap := TDrawEngine.Create;
  d := TDrawEngine(FDrawEngineMap);

  if (d.Rasterization.Memory.Width <> Width) or (d.Rasterization.Memory.Height <> Height) or (d.Rasterization.Memory.FBits <> FBits) then
    begin
      d.Rasterization.SetWorkMemory(Self);
      d.Rasterization.UsedAgg := True;
      d.Rasterization.Memory.Font := Font;
      d.ViewOptions := [];
    end;
  d.SetSize(Self);
  d.SetDrawInterfaceAsDefault;
  Result := d;
end;

procedure TMemoryRaster.ProjectionTo(Dst: TMemoryRaster; const sourRect, DestRect: TV2Rect4; const bilinear_sampling: Boolean; const alpha: TGeoFloat);
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
  R: TRectV2;
  nr: TMemoryRaster;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  var
    i: Integer;
  begin
    for i := 0 to Width - 1 do
      if (PointInRect(i, pass, R)) and (sour_Polygon.InHere(Vec2(i, pass))) then
          nr[i, pass] := PixelLinear[i, pass];
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    pass, i: Integer;
  begin
    for pass := 0 to Height - 1 do
      for i := 0 to Width - 1 do
        if (PointInRect(i, pass, R)) and (sour_Polygon.InHere(Vec2(i, pass))) then
            nr[i, pass] := PixelLinear[i, pass];
  end;
{$ENDIF Parallel}


begin
  nr := NewRaster();
  nr.SetSize(Width, Height, RColor(0, 0, 0, 0));
  R := sour_Polygon.BoundBox();

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMemoryRaster.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMemoryRaster.Parallel and LocalParallel, 0, Height - 1, procedure(pass: Integer)
    var
      i: Integer;
    begin
      for i := 0 to Width - 1 do
        if (PointInRect(i, pass, R)) and (sour_Polygon.InHere(Vec2(i, pass))) then
            nr[i, pass] := PixelLinear[i, pass];
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  nr.ProjectionTo(Dst, R, DestRect, bilinear_sampling, alpha);

  DisposeObject(nr);
end;

procedure TMemoryRaster.ProjectionPolygonTo(const sour_Polygon: T2DPolygonGraph; Dst: TMemoryRaster; DestRect: TRectV2; const bilinear_sampling: Boolean; const alpha: TGeoFloat);
var
  R: TRectV2;
  nr: TMemoryRaster;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  var
    i: Integer;
  begin
    for i := 0 to Width - 1 do
      if (PointInRect(i, pass, R)) and (sour_Polygon.InHere(Vec2(i, pass))) then
          nr[i, pass] := PixelLinear[i, pass];
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    pass, i: Integer;
  begin
    for pass := 0 to Height - 1 do
      for i := 0 to Width - 1 do
        if (PointInRect(i, pass, R)) and (sour_Polygon.InHere(Vec2(i, pass))) then
            nr[i, pass] := PixelLinear[i, pass];
  end;
{$ENDIF Parallel}


begin
  nr := NewRaster();
  nr.SetSize(Width, Height, RColor(0, 0, 0, 0));
  R := sour_Polygon.BoundBox();

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMemoryRaster.Parallel and LocalParallel, @Nested_ParallelFor, 0, Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMemoryRaster.Parallel and LocalParallel, 0, Height - 1, procedure(pass: Integer)
    var
      i: Integer;
    begin
      for i := 0 to Width - 1 do
        if (PointInRect(i, pass, R)) and (sour_Polygon.InHere(Vec2(i, pass))) then
            nr[i, pass] := PixelLinear[i, pass];
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  nr.ProjectionTo(Dst, R, DestRect, bilinear_sampling, alpha);
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

function TMemoryRaster.BuildMorphologySegmentation(OnGetPixelSegClassify: TOnGetPixelSegClassify): TMorphologySegmentation;
begin
  Result := TMorphologySegmentation.Create;
  Result.OnGetPixelSegClassify := OnGetPixelSegClassify;
  Result.BuildSegmentation(Self);
end;

function TMemoryRaster.BuildMorphologySegmentation(): TMorphologySegmentation;
begin
  Result := TMorphologySegmentation.Create;
  Result.BuildSegmentation(Self);
end;

procedure TMemoryRaster.BuildMorphomaticsTo(MorphPix_: TMorphologyPixel; output_: TMorphomatics);
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    output_.Bits^[pass] := RColorToMorph(FBits^[pass], MorphPix_);
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    pass: Integer;
  begin
    for pass := 0 to Width * Height - 1 do
      begin
        output_.Bits^[pass] := RColorToMorph(FBits^[pass], MorphPix_);
      end;
  end;
{$ENDIF Parallel}


begin
  ReadyBits();
  output_.SetSize(Width, Height);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMemoryRaster.Parallel and LocalParallel, @Nested_ParallelFor, 0, Width * Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMemoryRaster.Parallel and LocalParallel, 0, Width * Height - 1, procedure(pass: Integer)
    begin
      output_.Bits^[pass] := RColorToMorph(FBits^[pass], MorphPix_);
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
end;

function TMemoryRaster.BuildMorphomatics(MorphPix_: TMorphologyPixel): TMorphomatics;
begin
  Result := TMorphomatics.Create;
  Result.LocalParallel := LocalParallel;
  BuildMorphomaticsTo(MorphPix_, Result);
end;

procedure TMemoryRaster.BuildApproximateMorphomaticsTo(ApproximateColor_: TRColor; output_: TMorphomatics);
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    output_.Bits^[pass] := RColorToApproximateMorph(FBits^[pass], ApproximateColor_);
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    pass: Integer;
  begin
    for pass := 0 to Width * Height - 1 do
      begin
        output_.Bits^[pass] := RColorToApproximateMorph(FBits^[pass], ApproximateColor_);
      end;
  end;
{$ENDIF Parallel}


begin
  ReadyBits();
  output_.SetSize(Width, Height);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMemoryRaster.Parallel and LocalParallel, @Nested_ParallelFor, 0, Width * Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMemoryRaster.Parallel and LocalParallel, 0, Width * Height - 1, procedure(pass: Integer)
    begin
      output_.Bits^[pass] := RColorToApproximateMorph(FBits^[pass], ApproximateColor_);
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
end;

function TMemoryRaster.BuildApproximateMorphomatics(ApproximateColor_: TRColor): TMorphomatics;
begin
  Result := TMorphomatics.Create;
  Result.LocalParallel := LocalParallel;
  BuildApproximateMorphomaticsTo(ApproximateColor_, Result);
end;

procedure TMemoryRaster.DrawMorphomatics(MorphPix_: TMorphologyPixel; Morph: TMorphomatics);
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    MorphToRColor(MorphPix_, Morph.Bits^[pass], FBits^[pass]);
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    pass: Integer;
  begin
    for pass := 0 to Width * Height - 1 do
      begin
        MorphToRColor(MorphPix_, Morph.Bits^[pass], FBits^[pass]);
      end;
  end;
{$ENDIF Parallel}


begin
  if Morph.Width * Morph.Height <> Width * Height then
      SetSize(Morph.Width, Morph.Height, RColor(0, 0, 0, 0));

  ReadyBits();
{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMemoryRaster.Parallel and LocalParallel, @Nested_ParallelFor, 0, Width * Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMemoryRaster.Parallel and LocalParallel, 0, Width * Height - 1, procedure(pass: Integer)
    begin
      MorphToRColor(MorphPix_, Morph.Bits^[pass], FBits^[pass]);
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
end;

procedure TMemoryRaster.DrawMorphomatics(Morph: TMorphomatics);
begin
  DrawMorphomatics(mpGrayscale, Morph);
end;

procedure TMemoryRaster.DrawBinaryzation(Morph: TMorphologyBinaryzation);
var
  i: Integer;
begin
  if Morph.Width * Morph.Height <> Width * Height then
      SetSize(Width, Height);

  ReadyBits();
  for i := Width * Height - 1 downto 0 do
    if Morph.Bits^[i] then
        FBits^[i] := $FFFFFFFF
    else
        FBits^[i] := $FF000000;
end;

procedure TMemoryRaster.DrawBinaryzation(MorphPix_: TMorphologyPixel; Morph: TMorphologyBinaryzation);
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    MorphToRColor(MorphPix_, if_(Morph.Bits^[pass], 1.0, 0.0), FBits^[pass]);
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    pass: Integer;
  begin
    for pass := 0 to Width * Height - 1 do
      begin
        MorphToRColor(MorphPix_, if_(Morph.Bits^[pass], 1.0, 0.0), FBits^[pass]);
      end;
  end;
{$ENDIF Parallel}


begin
  if Morph.Width * Morph.Height <> Width * Height then
      SetSize(Morph.Width, Morph.Height, RColor(0, 0, 0, 0));

  ReadyBits();
{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(TMemoryRaster.Parallel and LocalParallel, @Nested_ParallelFor, 0, Width * Height - 1);
{$ELSE FPC}
  DelphiParallelFor(TMemoryRaster.Parallel and LocalParallel, 0, Width * Height - 1, procedure(pass: Integer)
    begin
      MorphToRColor(MorphPix_, if_(Morph.Bits^[pass], 1.0, 0.0), FBits^[pass]);
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
end;

function TMemoryRaster.BuildHistogram(MorphPix_: TMorphologyPixel; Height_: Integer; hColor: TRColor): TMemoryRaster;
var
  morph_: TMorphomatics;
begin
  morph_ := BuildMorphomatics(MorphPix_);
  Result := morph_.BuildHistogram(Height_, hColor);
  DisposeObject(morph_);
end;

class function TMemoryRaster.CanLoadStream(stream: TCoreClassStream): Boolean;
var
  bakPos: Int64;
  hflag, hflag2: Word;
  Header: TBmpHeader;
  j: TMemoryJpegRaster;
begin
  Result := False;
  if stream.Size < 8 then
      exit;
  try
    bakPos := stream.Position;

    if IsPNG(stream) then
        Result := True
    else
      begin
        stream.Position := bakPos;
        stream.Read(hflag, 2);
        if (hflag = $8D42)
          or (hflag = $8D43)
          or (hflag = $8D44)
          or (hflag = $8D46)
          or (hflag = $8D47) // half yuv
          or (hflag = $8D48) // quart yuv
          or (hflag = $8D50) // grayscale
          or (hflag = $8D51) // color 255
          or (hflag = $8D52) // color 65535
        then
            Result := True
        else if (hflag = $D8FF) or (hflag = $8DFF) then
            Result := True
        else
          begin
            stream.Position := bakPos;
            stream.ReadBuffer(Header, SizeOf(TBmpHeader));
            Result := (Header.bfType = $4D42) and (Header.biBitCount in [24, 32]) and (Header.biPlanes = 1) and (Header.biCompression = 0);
          end;
      end;

    stream.Position := bakPos;
  except
  end;
end;

procedure TMemoryRaster.LoadFromBmpStream(stream: TCoreClassStream);
var
  i, j, K, W: Integer;
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
              W := Width * 4;
              for i := Height - 1 downto 0 do
                begin
                  stream.ReadBuffer(ScanLine[i]^, W);
                end;
            end
          else
            begin
              stream.ReadBuffer(FBits^, Width * Height * 4);
            end;
        end
      else // 24bit bitmap
        begin
          W := FWidth * 3;
          K := W mod 4;
          if K <> 0 then
              inc(W, 4 - K);

          SetLength(tempBuff, W * FHeight);
          stream.ReadBuffer(tempBuff[0], W * FHeight);

          // Check whether the bitmap is saved top-down
          if Header.biHeight > 0 then
            begin
              K := 0;
              for j := Height - 1 downto 0 do
                begin
                  for i := 0 to FWidth - 1 do
                      FBits^[i + j * FWidth] := RGB2RGBA(PRGB(@tempBuff[K + i * 3])^);
                  inc(K, W);
                end;
            end
          else
            begin
              K := 0;
              for j := 0 to Height - 1 do
                begin
                  for i := 0 to FWidth - 1 do
                      FBits^[i + j * FWidth] := RGB2RGBA(PRGB(@tempBuff[K + i * 3])^);
                  inc(K, W);
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

  W, H: Integer;
  p: Pointer;
begin
  Reset;

  bakPos := stream.Position;

  if IsPNG(stream) then
    begin
      LoadRasterFromPNG(Self, stream);
      exit;
    end;

  stream.Position := bakPos;
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
    // jls endian/jpeg support
  else if (hflag = $D8FF) or (hflag = $8DFF) then
    begin
      stream.Read(hflag2, 2);
      stream.Position := bakPos;

      if (hflag2 = $F7FF) then
        // jls
          DecodeJpegLSRasterFromStream(stream, Self)
      else
        begin
          // jpeg
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
    // grapscale
  else if hflag = $8D50 then
    begin
      stream.Read(W, 4);
      stream.Read(H, 4);

      if stream is TMemoryStream64 then
        begin
          p := TMemoryStream64(stream).PositionAsPtr;
          DecryptGrayscale(W, H, p);
        end
      else
        begin
          p := System.GetMemory(W * H);
          stream.Read(p^, W * H);
          DecryptGrayscale(W, H, p);
          System.FreeMemory(p);
        end;
      exit;
    end
    // color 255
  else if hflag = $8D51 then
    begin
      stream.Read(W, 4);
      stream.Read(H, 4);

      if stream is TMemoryStream64 then
        begin
          p := TMemoryStream64(stream).PositionAsPtr;
          DecryptColor255(W, H, p);
        end
      else
        begin
          p := System.GetMemory(W * H);
          stream.Read(p^, W * H);
          DecryptColor255(W, H, p);
          System.FreeMemory(p);
        end;
      exit;
    end
    // color 65535
  else if hflag = $8D52 then
    begin
      stream.Read(W, 4);
      stream.Read(H, 4);

      if stream is TMemoryStream64 then
        begin
          p := TMemoryStream64(stream).PositionAsPtr;
          DecryptColor65535(W, H, p);
        end
      else
        begin
          p := System.GetMemory(W * H * 2);
          stream.Read(p^, W * H * 2);
          DecryptColor65535(W, H, p);
          System.FreeMemory(p);
        end;
      exit;
    end
  else
      stream.Position := bakPos;
end;

procedure TMemoryRaster.SaveToStream(stream: TCoreClassStream; RasterSave_: TRasterSaveFormat);
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
    rsJPEG_YCbCrA_Qualily90: SaveToJpegYCbCrAStream(stream, 90);
    rsJPEG_YCbCr_Qualily90: SaveToJpegYCbCrStream(stream, 90);
    rsJPEG_Gray_Qualily90: SaveToJPEGGrayStream(stream, 90);
    rsJPEG_GrayA_Qualily90: SaveToJPEGGrayAStream(stream, 90);
    rsJPEG_YCbCrA_Qualily80: SaveToJpegYCbCrAStream(stream, 80);
    rsJPEG_YCbCr_Qualily80: SaveToJpegYCbCrStream(stream, 80);
    rsJPEG_Gray_Qualily80: SaveToJPEGGrayStream(stream, 80);
    rsJPEG_GrayA_Qualily80: SaveToJPEGGrayAStream(stream, 80);
    rsJPEG_YCbCrA_Qualily70: SaveToJpegYCbCrAStream(stream, 70);
    rsJPEG_YCbCr_Qualily70: SaveToJpegYCbCrStream(stream, 70);
    rsJPEG_Gray_Qualily70: SaveToJPEGGrayStream(stream, 70);
    rsJPEG_GrayA_Qualily70: SaveToJPEGGrayAStream(stream, 70);
    rsJPEG_YCbCrA_Qualily60: SaveToJpegYCbCrAStream(stream, 60);
    rsJPEG_YCbCr_Qualily60: SaveToJpegYCbCrStream(stream, 60);
    rsJPEG_Gray_Qualily60: SaveToJPEGGrayStream(stream, 60);
    rsJPEG_GrayA_Qualily60: SaveToJPEGGrayAStream(stream, 60);
    rsJPEG_YCbCrA_Qualily50: SaveToJpegYCbCrAStream(stream, 50);
    rsJPEG_YCbCr_Qualily50: SaveToJpegYCbCrStream(stream, 50);
    rsJPEG_Gray_Qualily50: SaveToJPEGGrayStream(stream, 50);
    rsJPEG_GrayA_Qualily50: SaveToJPEGGrayAStream(stream, 50);
    rsJPEG_CMYK_Qualily90: SaveToJPEGCMYKStream(stream, 90);
    rsJPEG_CMYK_Qualily80: SaveToJPEGCMYKStream(stream, 80);
    rsJPEG_CMYK_Qualily70: SaveToJPEGCMYKStream(stream, 70);
    rsJPEG_CMYK_Qualily60: SaveToJPEGCMYKStream(stream, 60);
    rsJPEG_CMYK_Qualily50: SaveToJPEGCMYKStream(stream, 50);
    rsJPEG_YCbCrA_Qualily100: SaveToJpegYCbCrAStream(stream, 100);
    rsJPEG_YCbCr_Qualily100: SaveToJpegYCbCrStream(stream, 100);
    rsJPEG_Gray_Qualily100: SaveToJPEGGrayStream(stream, 100);
    rsJPEG_GrayA_Qualily100: SaveToJPEGGrayAStream(stream, 100);
    rsJPEG_CMYK_Qualily100: SaveToJPEGCMYKStream(stream, 100);
    rsPNG: SaveToPNGStream(stream);
    rsGrayscale: SaveToGrayscaleStream(stream);
    rsColor255: SaveToColor255Stream(stream);
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
  Header.biWidth := Width;

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

  for j := Height - 1 downto 0 do
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
  SaveToBmp32Stream(m64);
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
  SaveToBmp32Stream(m64);
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
  SaveToBmp32Stream(m64);
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

procedure TMemoryRaster.SaveToJpegYCbCrAStream(stream: TCoreClassStream; Quality: TJpegQuality);
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

procedure TMemoryRaster.SaveToJpegYCbCrStream(stream: TCoreClassStream; Quality: TJpegQuality);
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

procedure TMemoryRaster.SaveToJPEGCMYKStream(stream: TCoreClassStream; Quality: TJpegQuality);
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

procedure TMemoryRaster.SaveToGrayscaleStream(stream: TCoreClassStream);
var
  hflag: Word;
  W, H: Integer;
  p: PByteBuffer;
begin
  hflag := $8D50; // grayscale format
  stream.Write(hflag, 2);
  W := Width;
  H := Height;
  stream.Write(W, 4);
  stream.Write(H, 4);
  p := EncryptGrayscale();
  stream.Write(p^, W * H);
  System.FreeMemory(p);
end;

procedure TMemoryRaster.SaveToColor255Stream(stream: TCoreClassStream);
var
  hflag: Word;
  W, H: Integer;
  p: PByteBuffer;
begin
  hflag := $8D51; // color 255 format
  stream.Write(hflag, 2);
  W := Width;
  H := Height;
  stream.Write(W, 4);
  stream.Write(H, 4);
  p := EncryptColor255();
  stream.Write(p^, W * H);
  System.FreeMemory(p);
end;

procedure TMemoryRaster.SaveToColor65535Stream(stream: TCoreClassStream);
var
  hflag: Word;
  W, H: Integer;
  p: PWordBuffer;
begin
  hflag := $8D52; // color 255 format
  stream.Write(hflag, 2);
  W := Width;
  H := Height;
  stream.Write(W, 4);
  stream.Write(H, 4);
  p := EncryptColor65535();
  stream.Write(p^, W * H * 2);
  System.FreeMemory(p);
end;

procedure TMemoryRaster.SaveToPNGStream(stream: TCoreClassStream);
begin
  SaveRasterToPNG(Self, stream);
end;

class function TMemoryRaster.CanLoadFile(fn: SystemString): Boolean;
var
  m64: TCoreClassFileStream;
begin
  m64 := TCoreClassFileStream.Create(fn, fmOpenRead);
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
begin
  if umlMultipleMatch('*.jpg', fn) then
      SaveToJpegYCbCrFile(fn, 90)
  else if umlMultipleMatch('*.jpeg', fn) then
      SaveToJpegYCbCrFile(fn, 90)
  else if umlMultipleMatch('*.bmp', fn) then
      SaveToBmp32File(fn)
  else if umlMultipleMatch('*.yv12', fn) then
      SaveToYV12File(fn)
  else if umlMultipleMatch('*.jls', fn) then
      SaveToJpegLS3File(fn)
  else if umlMultipleMatch('*.hyuv', fn) then
      SaveToHalfYUVFile(fn)
  else if umlMultipleMatch('*.qyuv', fn) then
      SaveToQuartYUVFile(fn)
  else if umlMultipleMatch('*.png', fn) then
      SaveToPNGFile(fn)
  else if umlMultipleMatch('*.zlib_bmp', fn) then
      SaveToZLibCompressFile(fn)
  else if umlMultipleMatch('*.deflate_bmp', fn) then
      SaveToDeflateCompressFile(fn)
  else if umlMultipleMatch('*.BRRC_bmp', fn) then
      SaveToBRRCCompressFile(fn)
  else if umlMultipleMatch(['*.gray', '*.grayscale'], fn) then
      SaveToGrayscaleFile(fn)
  else if umlMultipleMatch(['*.255', '*.256'], fn) then
      SaveToColor255File(fn)
  else if umlMultipleMatch('*.64K', fn) then
      SaveToColor65535File(fn)
  else
      SaveToBmp32File(fn);
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

procedure TMemoryRaster.SaveToJpegYCbCrAFile(fn: SystemString; Quality: TJpegQuality);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToJpegYCbCrAStream(m64, Quality);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToJpegYCbCrFile(fn: SystemString; Quality: TJpegQuality);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToJpegYCbCrStream(m64, Quality);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToJpegCMYKFile(fn: SystemString; Quality: TJpegQuality);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToJPEGCMYKStream(m64, Quality);
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

procedure TMemoryRaster.SaveToGrayscaleFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToGrayscaleStream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToColor255File(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToColor255Stream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToColor65535File(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToColor65535Stream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

procedure TMemoryRaster.SaveToPNGFile(fn: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  try
      SaveToPNGStream(m64);
  except
  end;
  m64.SaveToFile(fn);
  DisposeObject(m64);
end;

constructor TMemoryRasterList.Create;
begin
  inherited Create;
  FCritical := TCritical.Create;
  AutoFreeRaster := False;
  UserToken := '';
end;

destructor TMemoryRasterList.Destroy;
begin
  DisposeObject(FCritical);
  Clear;
  inherited Destroy;
end;

procedure TMemoryRasterList.Lock;
begin
  FCritical.Lock;
end;

procedure TMemoryRasterList.UnLock;
begin
  FCritical.UnLock;
end;

procedure TMemoryRasterList.Remove(obj: TMemoryRaster);
begin
  if AutoFreeRaster then
      DisposeObject(obj);
  inherited Remove(obj);
end;

procedure TMemoryRasterList.Delete(index: Integer);
begin
  if (index >= 0) and (index < Count) then
    begin
      if AutoFreeRaster then
          DisposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TMemoryRasterList.Clear;
var
  i: Integer;
begin
  if AutoFreeRaster then
    for i := 0 to Count - 1 do
        DisposeObject(Items[i]);
  inherited Clear;
end;

function TMemoryRasterList.BuildArray: TMemoryRasterArray;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := Items[i];
end;

procedure TMemoryRasterList.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DisposeObject(Items[i]);
  Clear;
end;

constructor TMemoryRaster2DMatrix.Create;
begin
  inherited Create;
  FCritical := TCritical.Create;
  AutoFreeRaster := False;
end;

destructor TMemoryRaster2DMatrix.Destroy;
begin
  DisposeObject(FCritical);
  Clear;
  inherited Destroy;
end;

procedure TMemoryRaster2DMatrix.Lock;
begin
  FCritical.Lock;
end;

procedure TMemoryRaster2DMatrix.UnLock;
begin
  FCritical.UnLock;
end;

procedure TMemoryRaster2DMatrix.Remove(obj: TMemoryRasterList);
begin
  if AutoFreeRaster then
      DisposeObject(obj);
  inherited Remove(obj);
end;

procedure TMemoryRaster2DMatrix.Delete(index: Integer);
begin
  if (index >= 0) and (index < Count) then
    begin
      if AutoFreeRaster then
          DisposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TMemoryRaster2DMatrix.Clear;
var
  i: Integer;
begin
  if AutoFreeRaster then
    for i := 0 to Count - 1 do
        DisposeObject(Items[i]);
  inherited Clear;
end;

function TMemoryRaster2DMatrix.BuildArray: TMemoryRaster2DArray;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := Items[i].BuildArray;
end;

procedure TMemoryRaster2DMatrix.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    begin
      Items[i].Clean;
      DisposeObject(Items[i]);
    end;
  inherited Clear;
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
  C, i: Integer;
  br: TByteRaster;
  m64: TMemoryStream64;
begin
  Clear;
  d := TDataFrameEngine.Create;
  d.DecodeFrom(stream, True);

  C := d.Reader.ReadInteger;

  for i := 0 to C - 1 do
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
  // init
  FStream := stream_;
  FAutoFreeStream := False;
  FCritical := TCritical.Create;
  FWriteHistory := TMemoryRasterList.Create;
  FReadHistory := TMemoryRasterList.Create;
  FEnabledWriteHistory := False;
  FEnabledReadHistory := False;

  // update global Pool
  RasterSerializedPool.Lock;
  RasterSerializedPool.V.Add(Self);
  RasterSerializedPool.UnLock;
end;

destructor TRasterSerialized.Destroy;
begin
  // update global Pool
  RasterSerializedPool.Lock;
  RasterSerializedPool.V.Remove(Self);
  RasterSerializedPool.UnLock;

  // free
  ClearHistory();
  if FAutoFreeStream then
      DisposeObject(FStream);
  DisposeObject(FCritical);
  DisposeObject(FWriteHistory);
  DisposeObject(FReadHistory);
  inherited Destroy;
end;

function TRasterSerialized.Write(R: TMemoryRaster): Int64;
var
  h1, h2: TRasterSerializedHeader;
  p: Int64;
begin
  Result := 0;
  if (R = nil) or (R.empty()) then
      exit;

  FCritical.Acquire;
  try
    h1.Width := R.Width;
    h1.Height := R.Height;
    h1.siz := R.Width * R.Height * 4;
    h1.UsedAgg := R.FAggNeed;

    p := FStream.Size;
    if R.FMemorySerializedPosition >= 0 then
      begin
        FStream.Position := R.FMemorySerializedPosition;
        // overwrite
        if FStream.Read(h2, SizeOf(TRasterSerializedHeader)) = SizeOf(TRasterSerializedHeader) then
          if (h1.Width * h1.Height) = (h2.Width * h2.Height) then
              p := R.FMemorySerializedPosition;
      end;

    FStream.Position := p;

    // serialized write
    if FStream.Write(h1, SizeOf(TRasterSerializedHeader)) = SizeOf(TRasterSerializedHeader) then
      if FStream.Write(R.FBits^[0], h1.siz) = h1.siz then
        begin
          R.CloseVertex;
          R.FreeAgg;
          System.FreeMemory(R.FBits);
          R.FBits := nil;
          R.FMemorySerializedPosition := p;
          Result := h1.siz;
        end;

    if FEnabledWriteHistory then
      if FWriteHistory.IndexOf(R) < 0 then
          FWriteHistory.Add(R);
  finally
      FCritical.Release;
  end;
end;

function TRasterSerialized.Read(R: TMemoryRaster): Int64;
var
  H: TRasterSerializedHeader;
begin
  Result := 0;
  if (R = nil) or (R.FMemorySerializedPosition < 0) then
      exit;

  R.CloseVertex;
  R.FreeAgg;

  FCritical.Acquire;
  try
    if (R.FMemorySerializedPosition >= 0) then
      begin
        FStream.Position := R.FMemorySerializedPosition;
        if FStream.Read(H, SizeOf(TRasterSerializedHeader)) = SizeOf(TRasterSerializedHeader) then
          if FStream.Position + H.siz <= FStream.Size then
            begin
              // recycle memory change
              if Assigned(R.FBits) and R.FFreeBits then
                  System.FreeMemory(R.FBits);

              // alloc serialized memory
              R.FBits := System.GetMemory(H.siz);
              R.FWidth := H.Width;
              R.FHeight := H.Height;

              // restore
              if FStream.Read(R.FBits^[0], H.siz) = H.siz then
                begin
                  R.FFreeBits := True;
                  if H.UsedAgg then
                      R.OpenAgg;
                  Result := H.siz;
                end;
            end;
      end;

    if FEnabledReadHistory then
      if FReadHistory.IndexOf(R) < 0 then
          FReadHistory.Add(R);
  finally
      FCritical.Release;
  end;
end;

procedure TRasterSerialized.Remove(R: TMemoryRaster);
var
  i: Integer;
begin
  FCritical.Acquire;
  try
    i := 0;
    while i < FReadHistory.Count do
      begin
        if FReadHistory[i] = R then
            FReadHistory.Delete(i)
        else
            inc(i);
      end;

    i := 0;
    while i < FWriteHistory.Count do
      begin
        if FWriteHistory[i] = R then
            FWriteHistory.Delete(i)
        else
            inc(i);
      end;
  finally
      FCritical.Release;
  end;
end;

procedure TRasterSerialized.ClearHistory;
begin
  FCritical.Acquire;
  try
    FReadHistory.Clear;
    FWriteHistory.Clear;
  finally
      FCritical.Release;
  end;
end;

function TRasterSerialized.StreamSize: Int64;
begin
  FCritical.Acquire;
  Result := FStream.Size;
  FCritical.Release;
end;

function TRasterSerialized.StreamFile: U_String;
begin
  Result := '';
  FCritical.Acquire;
  if FStream is TCoreClassFileStream then
      Result := TCoreClassFileStream(FStream).FileName;
  FCritical.Release;
end;
