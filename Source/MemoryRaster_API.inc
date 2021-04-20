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

function ClampInt(const Value, IMin, IMax: Integer): Integer;
begin
  if Value > IMax then
      Result := IMax
  else if Value < IMin then
      Result := IMin
  else
      Result := Value;
end;

function ClampByte3(const Value, IMin, IMax: Byte): Byte;
begin
  if Value > IMax then
      Result := IMax
  else if Value < IMin then
      Result := IMin
  else
      Result := Value;
end;

function ClampByte(const Value: Cardinal): Byte;
begin
  if Value > $FF then
      Result := $FF
  else
      Result := Value;
end;

function ClampByte(const Value: Integer): Byte;
begin
  if Value > $FF then
      Result := $FF
  else if Value < 0 then
      Result := 0
  else
      Result := Value;
end;

function ClampByte(const Value: UInt64): Byte;
begin
  if Value > $FF then
      Result := $FF
  else
      Result := Value;
end;

function ClampByte(const Value: Int64): Byte;
begin
  if Value > $FF then
      Result := $FF
  else if Value < 0 then
      Result := 0
  else
      Result := Value;
end;

function RoundAsByte(const Value: Double): Byte;
begin
  Result := ClampByte(Round(Value));
end;

function IntersectRect_(out Dst: TRect; const r1, r2: TRect): Boolean;
begin
  if r1.Left >= r2.Left then
      Dst.Left := r1.Left
  else
      Dst.Left := r2.Left;
  if r1.Right <= r2.Right then
      Dst.Right := r1.Right
  else
      Dst.Right := r2.Right;
  if r1.Top >= r2.Top then
      Dst.Top := r1.Top
  else
      Dst.Top := r2.Top;
  if r1.Bottom <= r2.Bottom then
      Dst.Bottom := r1.Bottom
  else
      Dst.Bottom := r2.Bottom;
  Result := (Dst.Right >= Dst.Left) and (Dst.Bottom >= Dst.Top);
  if not Result then
      Dst := ZERO_RECT;
end;

procedure OffsetRect_(var R: TRect; dx, dy: Integer);
begin
  inc(R.Left, dx);
  inc(R.Top, dy);
  inc(R.Right, dx);
  inc(R.Bottom, dy);
end;

function IsRectEmpty_(const R: TRect): Boolean;
begin
  Result := (R.Right <= R.Left) or (R.Bottom <= R.Top);
end;

procedure DisposeRasterArray(var arry: TMemoryRasterArray);
var
  i: Integer;
begin
  for i := 0 to length(arry) - 1 do
    begin
      DisposeObject(arry[i]);
      arry[i] := nil;
    end;
end;

procedure BlendBlock(Dst: TMemoryRaster; dstRect: TRect; Src: TMemoryRaster; Srcx, Srcy: Integer; CombineOp: TDrawMode);
var
  SrcP, DstP: PRColor;
  sp, DP: PRColor;
  mc: TRColor;
  w, i, Dsty: Integer;
  BL: TBlendLine;
  ble: TBlendLineEx;
begin
  { Internal routine }
  w := dstRect.Right - dstRect.Left;
  SrcP := Src.PixelPtr[Srcx, Srcy];
  DstP := Dst.PixelPtr[dstRect.Left, dstRect.Top];

  case CombineOp of
    dmOpaque:
      begin
        for Dsty := dstRect.Top to dstRect.Bottom - 1 do
          begin
            CopyRColor(SrcP^, DstP^, w);
            inc(SrcP, Src.width);
            inc(DstP, Dst.width);
          end;
      end;
    dmBlend:
      if Src.MasterAlpha >= $FF then
        begin
          if Src.CombineMode = cmBlend then
              BL := {$IFDEF FPC}@{$ENDIF FPC}BlendLine
          else
              BL := {$IFDEF FPC}@{$ENDIF FPC}MergeLine;
          for Dsty := dstRect.Top to dstRect.Bottom - 1 do
            begin
              BL(SrcP, DstP, w);
              inc(SrcP, Src.width);
              inc(DstP, Dst.width);
            end
        end
      else
        begin
          if Src.CombineMode = cmBlend then
              ble := {$IFDEF FPC}@{$ENDIF FPC}BlendLineEx
          else
              ble := {$IFDEF FPC}@{$ENDIF FPC}MergeLineEx;
          for Dsty := dstRect.Top to dstRect.Bottom - 1 do
            begin
              ble(SrcP, DstP, w, Src.MasterAlpha);
              inc(SrcP, Src.width);
              inc(DstP, Dst.width);
            end
        end;
    dmTransparent:
      begin
        mc := Src.OuterColor;
        for Dsty := dstRect.Top to dstRect.Bottom - 1 do
          begin
            sp := SrcP;
            DP := DstP;
            { TODO: Write an optimized routine for fast masked transfers. }
            for i := 0 to w - 1 do
              begin
                if mc <> sp^ then
                    DP^ := sp^;
                inc(sp);
                inc(DP);
              end;
            inc(SrcP, Src.width);
            inc(DstP, Dst.width);
          end;
      end;
  end;
end;

procedure BlockTransfer(Dst: TMemoryRaster; Dstx: Integer; Dsty: Integer; DstClip: TRect; Src: TMemoryRaster; SrcRect: TRect; CombineOp: TDrawMode);
var
  Srcx, Srcy: Integer;
begin
  if Dst.Empty or Src.Empty or ((CombineOp = dmBlend) and (Src.MasterAlpha = 0)) then
      Exit;

  Srcx := SrcRect.Left;
  Srcy := SrcRect.Top;

  IntersectRect_(DstClip, DstClip, Dst.BoundsRect);
  IntersectRect_(SrcRect, SrcRect, Src.BoundsRect);

  OffsetRect_(SrcRect, Dstx - Srcx, Dsty - Srcy);
  IntersectRect_(SrcRect, DstClip, SrcRect);
  if IsRectEmpty_(SrcRect) then
      Exit;

  DstClip := SrcRect;
  OffsetRect_(SrcRect, Srcx - Dstx, Srcy - Dsty);

  BlendBlock(Dst, DstClip, Src, SrcRect.Left, SrcRect.Top, CombineOp);
end;

procedure FillRasterColor(BitPtr: Pointer; Count: Cardinal; Value: TRasterColor);
var
  p: PRasterColor;
begin
  p := PRasterColor(BitPtr);
  while Count > 0 do
    begin
      p^ := Value;
      inc(p);
      dec(Count);
    end;
end;

procedure CopyRasterColor(const Source; var dest; Count: Cardinal);
begin
  CopyPtr(@Source, @dest, Count shl 2);
end;

function RandomRasterColor(rand: TRandom; const A_, min_, max_: Byte): TRColor;
begin
  Result := RColor(
    ClampByte(umlRandomRange(rand, min_, max_)),
    ClampByte(umlRandomRange(rand, min_, max_)),
    ClampByte(umlRandomRange(rand, min_, max_)),
    A_);
end;

function RandomRasterColor(const A_, min_, max_: Byte): TRColor;
begin
  Result := RColor(
    ClampByte(umlRandomRange(min_, max_)),
    ClampByte(umlRandomRange(min_, max_)),
    ClampByte(umlRandomRange(min_, max_)),
    A_);
end;

function RandomRasterColor(const A: Byte): TRColor;
begin
  Result := RColor(
    ClampByte(umlRandomRange(0, $FF)),
    ClampByte(umlRandomRange(0, $FF)),
    ClampByte(umlRandomRange(0, $FF)),
    A);
end;

function RandomRasterColor(): TRColor;
begin
  Result := RandomRasterColor($FF);
end;

function RandomRasterColorF(const minR, maxR, minG, maxG, minB, maxB, minA, maxA: TGeoFloat): TRColor;
begin
  Result := RColorF(
    umlRandomRangeS(minR, maxR),
    umlRandomRangeS(minG, maxG),
    umlRandomRangeS(minB, maxB),
    umlRandomRangeS(minA, maxA));
end;

function RandomRasterColorF(rand: TRandom; minR, maxR, minG, maxG, minB, maxB, minA, maxA: TGeoFloat): TRColor;
begin
  Result := RColorF(
    umlRandomRangeS(rand, minR, maxR),
    umlRandomRangeS(rand, minG, maxG),
    umlRandomRangeS(rand, minB, maxB),
    umlRandomRangeS(rand, minA, maxA));
end;

function RasterColor(const v: TVec4): TRColor;
begin
  TRColorEntry(Result).R := ClampByte(Round(v[0] * $FF));
  TRColorEntry(Result).G := ClampByte(Round(v[1] * $FF));
  TRColorEntry(Result).B := ClampByte(Round(v[2] * $FF));
  TRColorEntry(Result).A := ClampByte(Round(v[3] * $FF));
end;

function RasterColor(const R, G, B, A: Byte): TRasterColor;
begin
  TRasterColorEntry(Result).R := R;
  TRasterColorEntry(Result).G := G;
  TRasterColorEntry(Result).B := B;
  TRasterColorEntry(Result).A := A;
end;

function RasterColor(const R, G, B: Byte): TRasterColor;
begin
  TRasterColorEntry(Result).R := R;
  TRasterColorEntry(Result).G := G;
  TRasterColorEntry(Result).B := B;
  TRasterColorEntry(Result).A := $FF;
end;

function RasterColorInv(const C: TRasterColor): TRasterColor;
begin
  TRasterColorEntry(Result).R := $FF - TRasterColorEntry(C).R;
  TRasterColorEntry(Result).G := $FF - TRasterColorEntry(C).G;
  TRasterColorEntry(Result).B := $FF - TRasterColorEntry(C).B;
  TRasterColorEntry(Result).A := TRasterColorEntry(C).A;
end;

function RasterAlphaColor(const C: TRasterColor; const A: Byte): TRasterColor;
begin
  if TRasterColorEntry(C).A = 0 then
      Result := 0
  else if TRasterColorEntry(C).A = $FF then
      Result := C
  else
    begin
      TRasterColorEntry(Result).R := Trunc(TRasterColorEntry(C).R * (TRasterColorEntry(C).A / $FF));
      TRasterColorEntry(Result).G := Trunc(TRasterColorEntry(C).G * (TRasterColorEntry(C).A / $FF));
      TRasterColorEntry(Result).B := Trunc(TRasterColorEntry(C).B * (TRasterColorEntry(C).A / $FF));
      TRasterColorEntry(Result).A := TRasterColorEntry(C).A;
    end;
end;

function RasterAlphaColorF(const C: TRasterColor; const A: TGeoFloat): TRasterColor;
begin
  Result := RasterAlphaColor(C, ClampByte(Trunc(A * $FF)));
end;

function RasterColorF(const R, G, B, A: TGeoFloat): TRasterColor;
begin
  TRasterColorEntry(Result).R := ClampByte(Round(R * $FF));
  TRasterColorEntry(Result).G := ClampByte(Round(G * $FF));
  TRasterColorEntry(Result).B := ClampByte(Round(B * $FF));
  TRasterColorEntry(Result).A := ClampByte(Round(A * $FF));
end;

function RasterColorF(const R, G, B: TGeoFloat): TRasterColor;
begin
  Result := RasterColorF(R, G, B, 1.0);
end;

procedure RasterColor2F(const C: TRasterColor; var R, G, B, A: TGeoFloat);
begin
  R := TRasterColorEntry(C).R / $FF;
  G := TRasterColorEntry(C).G / $FF;
  B := TRasterColorEntry(C).B / $FF;
  A := TRasterColorEntry(C).A / $FF;
end;

procedure RasterColor2F(const C: TRasterColor; var R, G, B: TGeoFloat);
begin
  R := TRasterColorEntry(C).R / $FF;
  G := TRasterColorEntry(C).G / $FF;
  B := TRasterColorEntry(C).B / $FF;
end;

function RasterColor2Vec4(const C: TRasterColor): TVec4;
begin
  RasterColor2F(C, Result[0], Result[1], Result[2], Result[3]);
end;

function RasterColor2Vector4(const C: TRasterColor): TVector4;
begin
  RasterColor2F(C, Result.Buff[0], Result.Buff[1], Result.Buff[2], Result.Buff[3]);
end;

function RasterColor2Vec3(const C: TRasterColor): TVec3;
begin
  RasterColor2F(C, Result[0], Result[1], Result[2]);
end;

function RasterColor2Vector3(const C: TRasterColor): TVector3;
begin
  RasterColor2F(C, Result.Buff[0], Result.Buff[1], Result.Buff[2]);
end;

function RasterColor2Gray(const C: TRasterColor): Byte;
begin
  with TRasterColorEntry(C) do
      Result := Round((R + G + B) / 3);
end;

function RasterColor2GrayS(const C: TRasterColor): TGeoFloat;
begin
  with TRasterColorEntry(C) do
      Result := (R + G + B) / 3 / $FF;
end;

function RasterColor2GrayD(const C: TRasterColor): Double;
begin
  with TRasterColorEntry(C) do
      Result := (R + G + B) / 3 / $FF;
end;

function SetRasterColorAlpha(const C: TRasterColor; const A: Byte): TRColor;
begin
  Result := C;
  TRasterColorEntry(Result).A := A;
end;

procedure FillRColor(BitPtr: Pointer; Count: Cardinal; Value: TRColor);
var
  p: PRColor;
begin
  p := PRColor(BitPtr);
  while Count > 0 do
    begin
      p^ := Value;
      inc(p);
      dec(Count);
    end;
end;

procedure CopyRColor(const Source; var dest; Count: Cardinal);
begin
  CopyPtr(@Source, @dest, Count shl 2);
end;

function RandomRColor(rand: TRandom; const A_, min_, max_: Byte): TRColor;
begin
  Result := RColor(
    ClampByte(umlRandomRange(rand, min_, max_)),
    ClampByte(umlRandomRange(rand, min_, max_)),
    ClampByte(umlRandomRange(rand, min_, max_)),
    A_);
end;

function RandomRColor(const A_, min_, max_: Byte): TRColor;
begin
  Result := RColor(
    ClampByte(umlRandomRange(min_, max_)),
    ClampByte(umlRandomRange(min_, max_)),
    ClampByte(umlRandomRange(min_, max_)),
    A_);
end;

function RandomRColor(const A: Byte): TRColor;
begin
  Result := RColor(
    ClampByte(umlRandomRange(0, $FF)),
    ClampByte(umlRandomRange(0, $FF)),
    ClampByte(umlRandomRange(0, $FF)),
    A);
end;

function RandomRColor(): TRColor;
begin
  Result := RandomRColor($FF);
end;

function RandomRColorF(const minR, maxR, minG, maxG, minB, maxB, minA, maxA: TGeoFloat): TRColor;
begin
  Result := RColorF(
    umlRandomRangeS(minR, maxR),
    umlRandomRangeS(minG, maxG),
    umlRandomRangeS(minB, maxB),
    umlRandomRangeS(minA, maxA));
end;

function RandomRColorF(rand: TRandom; minR, maxR, minG, maxG, minB, maxB, minA, maxA: TGeoFloat): TRColor;
begin
  Result := RColorF(
    umlRandomRangeS(rand, minR, maxR),
    umlRandomRangeS(rand, minG, maxG),
    umlRandomRangeS(rand, minB, maxB),
    umlRandomRangeS(rand, minA, maxA));
end;

function RColor(const s: U_String): TRColor;
var
  v, v1, v2, v3, v4: U_String;
  R, G, B, A: Byte;
begin
  v := umlTrimSpace(s);
  v1 := umlGetFirstStr(v, ', ');
  v := umlDeleteFirstStr(v, ', ');
  v2 := umlGetFirstStr(v, ', ');
  v := umlDeleteFirstStr(v, ', ');
  v3 := umlGetFirstStr(v, ', ');
  v := umlDeleteFirstStr(v, ', ');
  v4 := umlGetFirstStr(v, ', ');

  R := ClampByte(umlStrToInt(v1, 0));
  G := ClampByte(umlStrToInt(v2, 0));
  B := ClampByte(umlStrToInt(v3, 0));
  A := ClampByte(umlStrToInt(v4, $FF));
  Result := RColor(R, G, B, A);
end;

function RColor(const v: TVec4): TRColor;
begin
  TRColorEntry(Result).R := ClampByte(Round(v[0] * $FF));
  TRColorEntry(Result).G := ClampByte(Round(v[1] * $FF));
  TRColorEntry(Result).B := ClampByte(Round(v[2] * $FF));
  TRColorEntry(Result).A := ClampByte(Round(v[3] * $FF));
end;

function RColor(const R, G, B, A: Byte): TRColor;
begin
  TRColorEntry(Result).R := R;
  TRColorEntry(Result).G := G;
  TRColorEntry(Result).B := B;
  TRColorEntry(Result).A := A;
end;

function RColor(const R, G, B: Byte): TRColor;
begin
  TRColorEntry(Result).R := R;
  TRColorEntry(Result).G := G;
  TRColorEntry(Result).B := B;
  TRColorEntry(Result).A := $FF;
end;

function RColorInv(const C: TRColor): TRColor;
begin
  TRColorEntry(Result).R := $FF - TRColorEntry(C).R;
  TRColorEntry(Result).G := $FF - TRColorEntry(C).G;
  TRColorEntry(Result).B := $FF - TRColorEntry(C).B;
  TRColorEntry(Result).A := TRColorEntry(C).A;
end;

function RAlphaColor(const C: TRasterColor; const A: Byte): TRasterColor;
begin
  if TRasterColorEntry(C).A = 0 then
      Result := 0
  else if TRasterColorEntry(C).A = $FF then
      Result := C
  else
    begin
      TRasterColorEntry(Result).R := Trunc(TRasterColorEntry(C).R * (TRasterColorEntry(C).A / $FF));
      TRasterColorEntry(Result).G := Trunc(TRasterColorEntry(C).G * (TRasterColorEntry(C).A / $FF));
      TRasterColorEntry(Result).B := Trunc(TRasterColorEntry(C).B * (TRasterColorEntry(C).A / $FF));
      TRasterColorEntry(Result).A := TRasterColorEntry(C).A;
    end;
end;

function RAlphaColorF(const C: TRasterColor; const A: TGeoFloat): TRasterColor;
begin
  Result := RAlphaColor(C, ClampByte(Trunc(A * $FF)));
end;

function RColorF(const R, G, B, A: TGeoFloat): TRColor;
begin
  TRColorEntry(Result).R := ClampByte(Round(R * $FF));
  TRColorEntry(Result).G := ClampByte(Round(G * $FF));
  TRColorEntry(Result).B := ClampByte(Round(B * $FF));
  TRColorEntry(Result).A := ClampByte(Round(A * $FF));
end;

function RColorF(const R, G, B: TGeoFloat): TRColor;
begin
  Result := RColorF(R, G, B, 1.0);
end;

procedure RColor2F(const C: TRColor; var R, G, B, A: TGeoFloat);
begin
  R := TRColorEntry(C).R / $FF;
  G := TRColorEntry(C).G / $FF;
  B := TRColorEntry(C).B / $FF;
  A := TRColorEntry(C).A / $FF;
end;

procedure RColor2F(const C: TRColor; var R, G, B: TGeoFloat);
begin
  R := TRColorEntry(C).R / $FF;
  G := TRColorEntry(C).G / $FF;
  B := TRColorEntry(C).B / $FF;
end;

function RColor2Vec4(const C: TRColor): TVec4;
begin
  RColor2F(C, Result[0], Result[1], Result[2], Result[3]);
end;

function RColor2Vector4(const C: TRColor): TVector4;
begin
  RColor2F(C, Result.Buff[0], Result.Buff[1], Result.Buff[2], Result.Buff[3]);
end;

function RColor2Vec3(const C: TRColor): TVec3;
begin
  RColor2F(C, Result[0], Result[1], Result[2]);
end;

function RColor2Vector3(const C: TRColor): TVector3;
begin
  RColor2F(C, Result.Buff[0], Result.Buff[1], Result.Buff[2]);
end;

function RColor2Gray(const C: TRColor): Byte;
begin
  with TRColorEntry(C) do
      Result := Round((R + G + B) / 3);
end;

function RColor2GrayS(const C: TRColor): TGeoFloat;
begin
  with TRColorEntry(C) do
      Result := (R + G + B) / 3 / $FF;
end;

function RColor2GrayD(const C: TRColor): Double;
begin
  with TRColorEntry(C) do
      Result := (R + G + B) / 3 / $FF;
end;

function SetRColorAlpha(const C: TRasterColor; const A: Byte): TRColor;
begin
  Result := C;
  TRasterColorEntry(Result).A := A;
end;

function RColorDistanceMax(c1, c2: TRColor): Byte;
var
  ce1: TRColorEntry absolute c1;
  ce2: TRColorEntry absolute c2;
begin
  Result := 0;
  if ce1.R > ce2.R then
      Result := umlMax(Result, ce1.R - ce2.R)
  else
      Result := umlMax(Result, ce2.R - ce1.R);

  if ce1.G > ce2.G then
      Result := umlMax(Result, ce1.G - ce2.G)
  else
      Result := umlMax(Result, ce2.G - ce1.G);

  if ce1.B > ce2.B then
      Result := umlMax(Result, ce1.B - ce2.B)
  else
      Result := umlMax(Result, ce2.B - ce1.B);
end;

function RColorDistanceSum(c1, c2: TRColor): Integer;
var
  ce1: TRColorEntry absolute c1;
  ce2: TRColorEntry absolute c2;
begin
  Result := 0;
  if ce1.R > ce2.R then
      inc(Result, ce1.R - ce2.R)
  else
      inc(Result, ce2.R - ce1.R);

  if ce1.G > ce2.G then
      inc(Result, ce1.G - ce2.G)
  else
      inc(Result, ce2.G - ce1.G);

  if ce1.B > ce2.B then
      inc(Result, ce1.B - ce2.B)
  else
      inc(Result, ce2.B - ce1.B);
end;

function RColorDistance(c1, c2: TRColor): TGeoFloat;
begin
  Result := RColor2Vector3(c1).Distance3D(RColor2Vector3(c2));
end;

function RColorDistanceByte(c1, c2: TRColor): Byte;
begin
  Result := RoundAsByte(RColorDistance(c1, c2) * $FF);
end;

function RColorGradient(C: TRColor; level: Byte): TRColor;
var
  f: TGeoFloat;
  G: Byte;
begin
  f := $FF / umlMax(level, 1);
  G := Round(Trunc(RColor2Gray(C) / f) * f);
  Result := RColor(G, G, G);
end;

function RColorGrayGradient(C: TRColor; level: Byte): Byte;
var
  f: TGeoFloat;
begin
  f := $FF / umlMax(level, 1);
  Result := ClampByte(Round(Trunc(RColor2Gray(C) / f) * f));
end;

function RGBA2BGRA(const sour: TRColor): TRColor;
begin
  TRColorEntry(Result).R := TRColorEntry(sour).B;
  TRColorEntry(Result).G := TRColorEntry(sour).G;
  TRColorEntry(Result).B := TRColorEntry(sour).R;
  TRColorEntry(Result).A := TRColorEntry(sour).A;
end;

function BGRA2RGBA(const sour: TRColor): TRColor;
begin
  TRColorEntry(Result).R := TRColorEntry(sour).B;
  TRColorEntry(Result).G := TRColorEntry(sour).G;
  TRColorEntry(Result).B := TRColorEntry(sour).R;
  TRColorEntry(Result).A := TRColorEntry(sour).A;
end;

function RGBA2RGB(const sour: TRColor): TRGB;
begin
  Result := PRGB(@sour)^;
end;

function RGBA2BGR(const sour: TRColor): TRGB;
begin
  Result := PRGB(@sour)^;
  CoreClasses.Swap(Result[0], Result[2]);
end;

function RGB2BGR(const sour: TRGB): TRGB;
begin
  Result[0] := sour[2];
  Result[1] := sour[1];
  Result[2] := sour[0];
end;

function BGR2RGB(const sour: TRGB): TRGB;
begin
  Result[0] := sour[2];
  Result[1] := sour[1];
  Result[2] := sour[0];
end;

function RGB2RGBA(const sour: TRGB): TRColor;
begin
  PRGB(@Result)^ := sour;
  TRColorEntry(Result).A := $FF;
end;

procedure SwapBR(var sour: TRGB);
begin
  CoreClasses.Swap(sour[0], sour[2]);
end;

procedure SwapBR(var sour: TRColor);
begin
  CoreClasses.Swap(TRColorEntry(sour).R, TRColorEntry(sour).B);
end;

function MaxRGBComponent(sour: TRColor): Byte;
var
  C: TRColorEntry;
begin
  C.BGRA := sour;
  Result := Max(C.R, Max(C.G, C.B));
end;

function MaxRGBIndex(sour: TRColor): Byte;
var
  C: TRColorEntry;
begin
  C.BGRA := sour;
  if C.Buff[1] > C.Buff[0] then
    begin
      if C.Buff[2] > C.Buff[1] then
          Result := 2
      else
          Result := 1;
    end
  else
    begin
      if C.Buff[2] > C.Buff[0] then
          Result := 2
      else
          Result := 0;
    end;
end;

function MinRGBComponent(sour: TRColor): Byte;
var
  C: TRColorEntry;
begin
  C.BGRA := sour;
  Result := Min(C.R, Min(C.G, C.B));
end;

function MinRGBIndex(sour: TRColor): Byte;
var
  C: TRColorEntry;
begin
  C.BGRA := sour;
  if C.Buff[1] < C.Buff[0] then
    begin
      if C.Buff[2] < C.Buff[1] then
          Result := 2
      else
          Result := 1;
    end
  else
    begin
      if C.Buff[2] < C.Buff[0] then
          Result := 2
      else
          Result := 0;
    end;
end;

function RColorToApproximateMorph(Color, ApproximateColor_: TRColor): TMorphomaticsValue;
begin
  Result := (($FF * 3) - RColorDistanceSum(Color, ApproximateColor_)) / ($FF * 3);
end;

function RColorToMorph(Color: TRColor; MorphPix: TMorphologyPixel): TMorphomaticsValue;
var
  YIQ_: TYIQ;
  HSI_: THSI;
  CMYK_: TCMYK;
begin
  case MorphPix of
    mpGrayscale: Result := RColor2GrayD(Color);
    mpYIQ_Y:
      begin
        YIQ_.RGB := Color;
        Result := YIQ_.Y;
      end;
    mpYIQ_I:
      begin
        YIQ_.RGB := Color;
        Result := YIQ_.i;
      end;
    mpYIQ_Q:
      begin
        YIQ_.RGB := Color;
        Result := YIQ_.Q;
      end;
    mpHSI_H:
      begin
        HSI_.RGB := Color;
        Result := HSI_.H;
      end;
    mpHSI_S:
      begin
        HSI_.RGB := Color;
        Result := HSI_.s;
      end;
    mpHSI_I:
      begin
        HSI_.RGB := Color;
        Result := HSI_.i;
      end;
    mpCMYK_C:
      begin
        CMYK_.RGB := Color;
        Result := CMYK_.C;
      end;
    mpCMYK_M:
      begin
        CMYK_.RGB := Color;
        Result := CMYK_.M;
      end;
    mpCMYK_Y:
      begin
        CMYK_.RGB := Color;
        Result := CMYK_.Y;
      end;
    mpCMYK_K:
      begin
        CMYK_.RGB := Color;
        Result := CMYK_.K;
      end;
    mpR: Result := TRColorEntry(Color).R / $FF;
    mpG: Result := TRColorEntry(Color).G / $FF;
    mpB: Result := TRColorEntry(Color).B / $FF;
    mpA: Result := TRColorEntry(Color).A / $FF;
    mpApproximateBlack: Result := RColorToApproximateMorph(Color, RColor(0, 0, 0));
    mpApproximateWhite: Result := RColorToApproximateMorph(Color, RColor($FF, $FF, $FF));
    mpCyan: Result := RColorToApproximateMorph(Color, RColor(0, $FF, $FF));
    mpMagenta: Result := RColorToApproximateMorph(Color, RColor($FF, 0, $FF));
    mpYellow: Result := RColorToApproximateMorph(Color, RColor($FF, $FF, 0));
    else Result := 0;
  end;
end;

procedure MorphToRColor(MorphPix: TMorphologyPixel; Value: TMorphomaticsValue; var Color: TRColor);
var
  E: TRColorEntry;
  YIQ_: TYIQ;
  HSI_: THSI;
  CMYK_: TCMYK;
begin
  E.BGRA := Color;
  case MorphPix of
    mpGrayscale: E.BGRA := RColorF(Value, Value, Value, E.A / $FF);
    mpYIQ_Y:
      begin
        YIQ_.RGB := E.BGRA;
        YIQ_.Y := Value;
        E.BGRA := YIQ_.RGBA[E.A];
      end;
    mpYIQ_I:
      begin
        YIQ_.RGB := E.BGRA;
        YIQ_.i := Value;
        E.BGRA := YIQ_.RGBA[E.A];
      end;
    mpYIQ_Q:
      begin
        YIQ_.RGB := E.BGRA;
        YIQ_.Q := Value;
        E.BGRA := YIQ_.RGBA[E.A];
      end;
    mpHSI_H:
      begin
        HSI_.RGB := E.BGRA;
        HSI_.H := Value;
        E.BGRA := HSI_.RGBA[E.A];
      end;
    mpHSI_S:
      begin
        HSI_.RGB := E.BGRA;
        HSI_.s := Value;
        E.BGRA := HSI_.RGBA[E.A];
      end;
    mpHSI_I:
      begin
        HSI_.RGB := E.BGRA;
        HSI_.i := Value;
        E.BGRA := HSI_.RGBA[E.A];
      end;
    mpCMYK_C:
      begin
        CMYK_.RGB := E.BGRA;
        CMYK_.C := Value;
        E.BGRA := CMYK_.RGBA[E.A];
      end;
    mpCMYK_M:
      begin
        CMYK_.RGB := E.BGRA;
        CMYK_.M := Value;
        E.BGRA := CMYK_.RGBA[E.A];
      end;
    mpCMYK_Y:
      begin
        CMYK_.RGB := E.BGRA;
        CMYK_.Y := Value;
        E.BGRA := CMYK_.RGBA[E.A];
      end;
    mpCMYK_K:
      begin
        CMYK_.RGB := E.BGRA;
        CMYK_.K := Value;
        E.BGRA := CMYK_.RGBA[E.A];
      end;
    mpR: E.R := ClampByte(Round(Value * $FF));
    mpG: E.G := ClampByte(Round(Value * $FF));
    mpB: E.B := ClampByte(Round(Value * $FF));
    mpA: E.A := ClampByte(Round(Value * $FF));
    mpApproximateBlack: E.BGRA := RColorF(Value, Value, Value);
    mpApproximateWhite: E.BGRA := RColorF(Value, Value, Value);
    mpCyan: E.BGRA := RColorF(0, Value, Value);
    mpMagenta: E.BGRA := RColorF(Value, 0, Value);
    mpYellow: E.BGRA := RColorF(Value, Value, 0);
    else RaiseInfo('error');
  end;
  Color := E.BGRA;
end;

{$IFDEF OverflowCheck}{$Q-}{$ENDIF}
{$IFDEF RangeCheck}{$R-}{$ENDIF}


procedure FillColorTable(const RBits, GBits, BBits: Byte; const DestCount: Integer; dest: PRColorArray);
var
  i, TotalBits, MaxCount_: Integer;
begin
  TotalBits := RBits + GBits + BBits;
  MaxCount_ := umlMin(1 shl TotalBits, DestCount);
  FillPtr(@dest^[0], DestCount * SizeOf(TRColor), 0);

  for i := 0 to MaxCount_ - 1 do
      dest^[i] :=
      RColor(((i shr Max(0, GBits + BBits - 1)) and (1 shl RBits - 1)) * $FF div (1 shl RBits - 1),
      ((i shr Max(0, BBits - 1)) and (1 shl GBits - 1)) * $FF div (1 shl GBits - 1),
      ((i shr 0) and (1 shl BBits - 1)) * $FF div (1 shl BBits - 1));
end;
{$IFDEF OverflowCheck}{$Q+}{$ENDIF}
{$IFDEF RangeCheck}{$R+}{$ENDIF}


function FindColorIndex(Color: TRColor; const DestCount: Integer; dest: PRColorArray): Integer;
var
  Col: TRColorEntry;
  i, MinDif, Dif: Integer;
begin
  Result := -1;
  Col.BGRA := Color;
  Col.A := $FF;

  // serach nearest
  MinDif := $03FC;
  for i := 0 to DestCount - 1 do
    with PRColorEntry(@dest^[i])^ do
      begin
        Dif := Abs(R - Col.R);
        if Dif > MinDif then
            Continue;
        Dif := Dif + Abs(G - Col.G);
        if Dif > MinDif then
            Continue;
        Dif := Dif + Abs(B - Col.B);
        if Dif > MinDif then
            Continue;
        if Dif < MinDif then
          begin
            MinDif := Dif;
            Result := i;
            if MinDif = 0 then
                Exit;
          end;
      end;
end;

function FindColor255Index(Color: TRColor): Integer;
begin
  Result := FindColorIndex(Color, $FF, @Color255);
end;

function FindColor65535Index(Color: TRColor): Integer;
begin
  Result := FindColorIndex(Color, $FFFF, @Color65535);
end;

function AggColor(const Value: TRColor): TAggColorRgba8;
begin
  Result.R := TRColorEntry(Value).R;
  Result.G := TRColorEntry(Value).G;
  Result.B := TRColorEntry(Value).B;
  Result.A := TRColorEntry(Value).A;
end;

function AggColor(const R, G, B: TGeoFloat; const A: TGeoFloat = 1.0): TAggColorRgba8;
begin
  Result := AggColor(RColorF(R, G, B, A));
end;

function AggColor(const Value: TAggColorRgba8): TRColor;
begin
  TRColorEntry(Result).R := Value.R;
  TRColorEntry(Result).G := Value.G;
  TRColorEntry(Result).B := Value.B;
  TRColorEntry(Result).A := Value.A;
end;

function ComputeSize(const MAX_Width, MAX_Height: Integer; var width, height: Integer): TGeoFloat;
var
  fw, fh: TGeoFloat;
begin
  fw := MAX_Width / width;
  fh := MAX_Height / height;

  if fw < fh then
      Result := fw
  else
      Result := fh;

  width := Trunc(width * Result);
  height := Trunc(height * Result);
end;

procedure FastBlur(Source, dest: TMemoryRaster; radius: Double; const Bounds: TRect);
type
  TSumRecord = record
    B, G, R, A, Sum: Integer;
  end;
var
  LL, RR, TT, BB, xx, yy, i, j, X, Y, RadiusI, Passes: Integer;
  RecLeft, RecTop, RecRight, RecBottom: Integer;
  ImagePixel: PRColorEntry;
  SumRec: TSumRecord;
  ImgPixel: PRColorEntry;
  pixels: array of TRColorEntry;
begin
  if dest <> Source then
      dest.Assign(Source);

  if radius < 1 then
      Exit
  else if radius > 256 then
      radius := 256;

  RadiusI := Round(radius / Sqrt(-2 * ln(1 / $FF)));
  if RadiusI < 2 then
    begin
      Passes := Round(radius);
      RadiusI := 1;
    end
  else
      Passes := 3;

  RecLeft := Max(Bounds.Left, 0);
  RecTop := Max(Bounds.Top, 0);
  RecRight := Min(Bounds.Right, dest.width - 1);
  RecBottom := Min(Bounds.Bottom, dest.height - 1);

  SetLength(pixels, Max(dest.width, dest.height) + 1);

  // pre-multiply alphas ...
  for Y := RecTop to RecBottom do
    begin
      ImgPixel := PRColorEntry(dest.ScanLine[Y]);
      inc(ImgPixel, RecLeft);
      for X := RecLeft to RecRight do
        with ImgPixel^ do
          begin
            R := DivTable[R, A];
            G := DivTable[G, A];
            B := DivTable[B, A];
            inc(ImgPixel);
          end;
    end;

  for i := 1 to Passes do
    begin
      // horizontal pass...
      for Y := RecTop to RecBottom do
        begin
          ImagePixel := PRColorEntry(@dest.ScanLine[Y]^[RecLeft]);
          // fill the Pixels buffer with a copy of the row's pixels ...
          CopyRColor(ImagePixel^, pixels[RecLeft], RecRight - RecLeft + 1);

          SumRec.A := 0;
          SumRec.R := 0;
          SumRec.G := 0;
          SumRec.B := 0;
          SumRec.Sum := 0;

          LL := RecLeft;
          RR := RecLeft + RadiusI;
          if RR > RecRight then
              RR := RecRight;
          // update first in row ...
          for xx := LL to RR do
            with pixels[xx] do
              begin
                inc(SumRec.A, A);
                inc(SumRec.R, R);
                inc(SumRec.G, G);
                inc(SumRec.B, B);
                inc(SumRec.Sum);
              end;
          with ImagePixel^ do
            begin
              A := SumRec.A div SumRec.Sum;
              R := SumRec.R div SumRec.Sum;
              G := SumRec.G div SumRec.Sum;
              B := SumRec.B div SumRec.Sum;
            end;
          // update the remaining pixels in the row ...
          for X := RecLeft + 1 to RecRight do
            begin
              inc(ImagePixel);
              LL := X - RadiusI - 1;
              RR := X + RadiusI;
              if LL >= RecLeft then
                with pixels[LL] do
                  begin
                    dec(SumRec.A, A);
                    dec(SumRec.R, R);
                    dec(SumRec.G, G);
                    dec(SumRec.B, B);
                    dec(SumRec.Sum);
                  end;
              if RR <= RecRight then
                with pixels[RR] do
                  begin
                    inc(SumRec.A, A);
                    inc(SumRec.R, R);
                    inc(SumRec.G, G);
                    inc(SumRec.B, B);
                    inc(SumRec.Sum);
                  end;
              with ImagePixel^ do
                begin
                  A := SumRec.A div SumRec.Sum;
                  R := SumRec.R div SumRec.Sum;
                  G := SumRec.G div SumRec.Sum;
                  B := SumRec.B div SumRec.Sum;
                end;
            end;
        end;

      // vertical pass...
      for X := RecLeft to RecRight do
        begin
          ImagePixel := PRColorEntry(@dest.ScanLine[RecTop]^[X]);
          for j := RecTop to RecBottom do
            begin
              pixels[j] := ImagePixel^;
              inc(ImagePixel, dest.width);
            end;
          ImagePixel := PRColorEntry(@dest.ScanLine[RecTop]^[X]);

          TT := RecTop;
          BB := RecTop + RadiusI;
          if BB > RecBottom then
              BB := RecBottom;
          SumRec.A := 0;
          SumRec.R := 0;
          SumRec.G := 0;
          SumRec.B := 0;
          SumRec.Sum := 0;
          // update first in col ...
          for yy := TT to BB do
            with pixels[yy] do
              begin
                inc(SumRec.A, A);
                inc(SumRec.R, R);
                inc(SumRec.G, G);
                inc(SumRec.B, B);
                inc(SumRec.Sum);
              end;
          with ImagePixel^ do
            begin
              A := SumRec.A div SumRec.Sum;
              R := SumRec.R div SumRec.Sum;
              G := SumRec.G div SumRec.Sum;
              B := SumRec.B div SumRec.Sum;
            end;
          // update remainder in col ...
          for Y := RecTop + 1 to RecBottom do
            begin
              inc(ImagePixel, dest.width);
              TT := Y - RadiusI - 1;
              BB := Y + RadiusI;

              if TT >= RecTop then
                with pixels[TT] do
                  begin
                    dec(SumRec.A, A);
                    dec(SumRec.R, R);
                    dec(SumRec.G, G);
                    dec(SumRec.B, B);
                    dec(SumRec.Sum);
                  end;
              if BB <= RecBottom then
                with pixels[BB] do
                  begin
                    inc(SumRec.A, A);
                    inc(SumRec.R, R);
                    inc(SumRec.G, G);
                    inc(SumRec.B, B);
                    inc(SumRec.Sum);
                  end;
              with ImagePixel^ do
                begin
                  A := SumRec.A div SumRec.Sum;
                  R := SumRec.R div SumRec.Sum;
                  G := SumRec.G div SumRec.Sum;
                  B := SumRec.B div SumRec.Sum;
                end;
            end;
        end;
    end;

  // extract alphas ...
  for Y := RecTop to RecBottom do
    begin
      ImgPixel := PRColorEntry(@dest.ScanLine[Y]^[RecLeft]);
      for X := RecLeft to RecRight do
        begin
          ImgPixel^.R := RcTable[ImgPixel^.A, ImgPixel^.R];
          ImgPixel^.G := RcTable[ImgPixel^.A, ImgPixel^.G];
          ImgPixel^.B := RcTable[ImgPixel^.A, ImgPixel^.B];
          inc(ImgPixel);
        end;
    end;
end;

procedure FastBlur(Source: TMemoryRaster; radius: Double; const Bounds: TRect);
begin
  FastBlur(Source, Source, radius, Bounds);
end;

procedure GaussianBlur(Source, dest: TMemoryRaster; radius: Double; const Bounds: TRect);
const
  ChannelSize = 256;
  ChannelSizeMin1 = ChannelSize - 1;
type
  TSumRecInt64 = record
    B, G, R, A: Int64;
    Sum: Integer;
  end;
var
  Q, i, j, X, Y, ImageWidth, RowOffset, RadiusI: Integer;
  RecLeft, RecTop, RecRight, RecBottom: Integer;
  ImagePixels: PRColorEntryArray;
  RadiusSq, RadiusRevSq, KernelSize: Integer;
  SumRec: TSumRecInt64;
  PreMulArray: array of TRColorEntry;
  SumArray: array of TSumRecInt64;
  GaussLUT: array of array of Cardinal;
  SourPixel, DestPixel: PRColorEntry;
begin
  Source.ReadyBits();
  dest.ReadyBits();

  RadiusI := Round(radius);
  if RadiusI < 1 then
    begin
      if Source <> dest then
          dest.Assign(Source);
      Exit;
    end
  else if RadiusI > 128 then
      RadiusI := 128; // nb: performance degrades exponentially with >> Radius

  // initialize the look-up-table ...
  KernelSize := RadiusI * 2 + 1;
  SetLength(GaussLUT, KernelSize);
  for i := 0 to KernelSize - 1 do
      SetLength(GaussLUT[i], ChannelSize);
  for i := 1 to RadiusI do
    begin
      RadiusRevSq := Round((radius + 1 - i) * (radius + 1 - i));
      for j := 0 to ChannelSizeMin1 do
        begin
          GaussLUT[RadiusI - i][j] := RadiusRevSq * j;
          GaussLUT[RadiusI + i][j] := GaussLUT[RadiusI - i][j];
        end;
    end;
  RadiusSq := Round((radius + 1) * (radius + 1));
  for j := 0 to ChannelSizeMin1 do
      GaussLUT[RadiusI][j] := RadiusSq * j;

  ImageWidth := Source.width;
  SetLength(SumArray, ImageWidth * Source.height);

  ImagePixels := PRColorEntryArray(Source.FBits);
  RecLeft := Max(Bounds.Left, 0);
  RecTop := Max(Bounds.Top, 0);
  RecRight := Min(Bounds.Right, ImageWidth - 1);
  RecBottom := Min(Bounds.Bottom, Source.height - 1);

  RowOffset := RecTop * ImageWidth;
  SetLength(PreMulArray, Source.width);
  for Y := RecTop to RecBottom do
    begin
      // initialize PreMulArray for the row ...
      Q := (Y * ImageWidth) + RecLeft;
      for X := RecLeft to RecRight do
        with ImagePixels^[Q] do
          begin
            PreMulArray[X].A := A;
            PreMulArray[X].R := DivTable[R, A];
            PreMulArray[X].G := DivTable[G, A];
            PreMulArray[X].B := DivTable[B, A];
            inc(Q);
          end;

      for X := RecLeft to RecRight do
        begin
          SumRec.A := 0;
          SumRec.R := 0;
          SumRec.G := 0;
          SumRec.B := 0;
          SumRec.Sum := 0;

          i := Max(X - RadiusI, RecLeft);
          Q := i - (X - RadiusI);
          for i := i to Min(X + RadiusI, RecRight) do
            with PreMulArray[i] do
              begin
                inc(SumRec.A, GaussLUT[Q][A]);
                inc(SumRec.R, GaussLUT[Q][R]);
                inc(SumRec.G, GaussLUT[Q][G]);
                inc(SumRec.B, GaussLUT[Q][B]);
                inc(SumRec.Sum, GaussLUT[Q][1]);
                inc(Q);
              end;
          Q := RowOffset + X;
          SumArray[Q].A := SumRec.A div SumRec.Sum;
          SumArray[Q].R := SumRec.R div SumRec.Sum;
          SumArray[Q].G := SumRec.G div SumRec.Sum;
          SumArray[Q].B := SumRec.B div SumRec.Sum;
        end;
      inc(RowOffset, ImageWidth);
    end;

  if Source <> dest then
      dest.SetSize(Source.width, Source.height);

  RowOffset := RecTop * ImageWidth;
  for Y := RecTop to RecBottom do
    begin
      for X := RecLeft to RecRight do
        begin
          SumRec.A := 0;
          SumRec.R := 0;
          SumRec.G := 0;
          SumRec.B := 0;
          SumRec.Sum := 0;

          i := Max(Y - RadiusI, RecTop);
          Q := i - (Y - RadiusI);
          for i := i to Min(Y + RadiusI, RecBottom) do
            with SumArray[X + i * ImageWidth] do
              begin
                inc(SumRec.A, GaussLUT[Q][A]);
                inc(SumRec.R, GaussLUT[Q][R]);
                inc(SumRec.G, GaussLUT[Q][G]);
                inc(SumRec.B, GaussLUT[Q][B]);
                inc(SumRec.Sum, GaussLUT[Q][1]);
                inc(Q);
              end;

          SourPixel := @ImagePixels^[RowOffset + X];

          if Source <> dest then
              DestPixel := @PRColorEntryArray(dest.FBits)^[RowOffset + X]
          else
              DestPixel := SourPixel;

          DestPixel^.A := (SumRec.A div SumRec.Sum);
          DestPixel^.R := RcTable[DestPixel^.A, (SumRec.R div SumRec.Sum)];
          DestPixel^.G := RcTable[DestPixel^.A, (SumRec.G div SumRec.Sum)];
          DestPixel^.B := RcTable[DestPixel^.A, (SumRec.B div SumRec.Sum)];
        end;
      inc(RowOffset, ImageWidth);
    end;
end;

procedure GaussianBlur(Source: TMemoryRaster; radius: Double; const Bounds: TRect);
begin
  GaussianBlur(Source, Source, radius, Bounds);
end;

procedure GrayscaleBlur(Source, dest: TMemoryRaster; radius: Double; const Bounds: TRect);
const
  ChannelSize = 256; // ie 1 byte for each of A,R,G & B in TRColor
  ChannelSizeMin1 = ChannelSize - 1;
type
  TSumRecInt64 = record
    R, A: Int64;
    Sum: Integer;
  end;
var
  Q, i, j, X, Y, ImageWidth, RowOffset, RadiusI: Integer;
  RecLeft, RecTop, RecRight, RecBottom: Integer;
  ImagePixels: PRColorEntryArray;
  RadiusSq, RadiusRevSq, KernelSize: Integer;
  SumRec: TSumRecInt64;
  PreMulArray: array of TRColorEntry;
  SumArray: array of TSumRecInt64;
  GaussLUT: array of array of Cardinal;
  SourPixel, DestPixel: PRColorEntry;
begin
  Source.ReadyBits();
  dest.ReadyBits();

  RadiusI := Round(radius);
  if RadiusI < 1 then
    begin
      if Source <> dest then
          dest.Assign(Source);

      dest.Grayscale;
      Exit;
    end
  else if RadiusI > 128 then
      RadiusI := 128; // nb: performance degrades exponentially with >> Radius

  // initialize the look-up-table ...
  KernelSize := RadiusI * 2 + 1;
  SetLength(GaussLUT, KernelSize);
  for i := 0 to KernelSize - 1 do
      SetLength(GaussLUT[i], ChannelSize);
  for i := 1 to RadiusI do
    begin
      RadiusRevSq := Round((radius + 1 - i) * (radius + 1 - i));
      for j := 0 to ChannelSizeMin1 do
        begin
          GaussLUT[RadiusI - i][j] := RadiusRevSq * j;
          GaussLUT[RadiusI + i][j] := GaussLUT[RadiusI - i][j];
        end;
    end;
  RadiusSq := Round((radius + 1) * (radius + 1));
  for j := 0 to ChannelSizeMin1 do
      GaussLUT[RadiusI][j] := RadiusSq * j;

  ImageWidth := Source.width;
  SetLength(SumArray, ImageWidth * Source.height);

  ImagePixels := PRColorEntryArray(Source.FBits);
  RecLeft := Max(Bounds.Left, 0);
  RecTop := Max(Bounds.Top, 0);
  RecRight := Min(Bounds.Right, ImageWidth - 1);
  RecBottom := Min(Bounds.Bottom, Source.height - 1);

  RowOffset := RecTop * ImageWidth;
  SetLength(PreMulArray, Source.width);
  for Y := RecTop to RecBottom do
    begin
      // initialize PreMulArray for the row ...
      Q := (Y * ImageWidth) + RecLeft;
      for X := RecLeft to RecRight do
        with ImagePixels^[Q] do
          begin
            PreMulArray[X].A := A;
            PreMulArray[X].R := DivTable[RColor2Gray(BGRA), A];
            inc(Q);
          end;

      for X := RecLeft to RecRight do
        begin
          SumRec.A := 0;
          SumRec.R := 0;
          SumRec.Sum := 0;

          i := Max(X - RadiusI, RecLeft);
          Q := i - (X - RadiusI);
          for i := i to Min(X + RadiusI, RecRight) do
            with PreMulArray[i] do
              begin
                inc(SumRec.A, GaussLUT[Q][A]);
                inc(SumRec.R, GaussLUT[Q][R]);
                inc(SumRec.Sum, GaussLUT[Q][1]);
                inc(Q);
              end;
          Q := RowOffset + X;
          SumArray[Q].A := SumRec.A div SumRec.Sum;
          SumArray[Q].R := SumRec.R div SumRec.Sum;
        end;
      inc(RowOffset, ImageWidth);
    end;

  if Source <> dest then
      dest.SetSize(Source.width, Source.height);

  RowOffset := RecTop * ImageWidth;
  for Y := RecTop to RecBottom do
    begin
      for X := RecLeft to RecRight do
        begin
          SumRec.A := 0;
          SumRec.R := 0;
          SumRec.Sum := 0;

          i := Max(Y - RadiusI, RecTop);
          Q := i - (Y - RadiusI);
          for i := i to Min(Y + RadiusI, RecBottom) do
            with SumArray[X + i * ImageWidth] do
              begin
                inc(SumRec.A, GaussLUT[Q][A]);
                inc(SumRec.R, GaussLUT[Q][R]);
                inc(SumRec.Sum, GaussLUT[Q][1]);
                inc(Q);
              end;

          SourPixel := @ImagePixels^[RowOffset + X];

          if Source <> dest then
              DestPixel := @PRColorEntryArray(dest.FBits)^[RowOffset + X]
          else
              DestPixel := SourPixel;

          DestPixel^.A := (SumRec.A div SumRec.Sum);
          DestPixel^.R := RcTable[DestPixel^.A, (SumRec.R div SumRec.Sum)];
          DestPixel^.G := DestPixel^.R;
          DestPixel^.B := DestPixel^.G;
        end;
      inc(RowOffset, ImageWidth);
    end;
end;

procedure GrayscaleBlur(Source: TMemoryRaster; radius: Double; const Bounds: TRect);
begin
  GrayscaleBlur(Source, Source, radius, Bounds);
end;

procedure Antialias32(const DestMR: TMemoryRaster; AXOrigin, AYOrigin, AXFinal, AYFinal: Integer);
var
  LMemo, X, Y: Integer;
  A, R, G, B: Cardinal;
  A0, A1Prev, A1Next, a2: Cardinal;
  r0, R1Prev, R1Next, r2: Cardinal;
  G0, G1Prev, G1Next, g2: Cardinal;
  b0, B1Prev, B1Next, b2: Cardinal;
  P0, p1, p2: PRColorArray;
begin
  if AXFinal < AXOrigin then
    begin
      LMemo := AXOrigin;
      AXOrigin := AXFinal;
      AXFinal := LMemo;
    end;

  if AYFinal < AYOrigin then
    begin
      LMemo := AYOrigin;
      AYOrigin := AYFinal;
      AYFinal := LMemo;
    end;

  AXOrigin := Max(1, AXOrigin);
  AYOrigin := Max(1, AYOrigin);
  AXFinal := Min(DestMR.width - 2, AXFinal);
  AYFinal := Min(DestMR.height - 2, AYFinal);

  for Y := AYOrigin to AYFinal do
    begin
      P0 := DestMR.ScanLine[Y - 1];
      p1 := DestMR.ScanLine[Y];
      p2 := DestMR.ScanLine[Y + 1];

      for X := AXOrigin to AXFinal do
        begin
          // alpha component
          A0 := P0^[X] shr 24 and $FF;
          A1Prev := p1^[X - 1] shr 24 and $FF;
          A1Next := p1^[X + 1] shr 24 and $FF;
          a2 := p2^[X] shr 24 and $FF;

          // red component
          r0 := P0^[X] shr 16 and $FF;
          R1Prev := p1^[X - 1] shr 16 and $FF;
          R1Next := p1^[X + 1] shr 16 and $FF;
          r2 := p2^[X] shr 16 and $FF;

          // green component
          G0 := P0^[X] shr 8 and $FF;
          G1Prev := p1^[X - 1] shr 8 and $FF;
          G1Next := p1^[X + 1] shr 8 and $FF;
          g2 := p2^[X] shr 8 and $FF;

          // blue component
          b0 := P0^[X] and $FF;
          B1Prev := p1^[X - 1] and $FF;
          B1Next := p1^[X + 1] and $FF;
          b2 := p2^[X] and $FF;

          // composition
          A := (A0 + a2 + A1Prev + A1Next) div 4;
          R := (r0 + r2 + R1Prev + R1Next) div 4;
          G := (G0 + g2 + G1Prev + G1Next) div 4;
          B := (b0 + b2 + B1Prev + B1Next) div 4;

          p1^[X] := (A shl 24) or (R shl 16) or (G shl 8) or B;
        end;
    end;
end;

procedure Antialias32(const DestMR: TMemoryRaster; const Amount_: Integer);
var
  i: Integer;
begin
  if Amount_ >= 1 then
    for i := 1 to Amount_ do
        Antialias32(DestMR, 0, 0, DestMR.width, DestMR.height);
end;

procedure Antialias32(const DestMR: TMemoryRaster);
begin
  Antialias32(DestMR, 0, 0, DestMR.width, DestMR.height);
end;

procedure HistogramEqualize(const mr: TMemoryRaster);
var
  LHistogram: array [0 .. $FF] of Cardinal;
  LMap: array [0 .. $FF] of Byte;
  i: Integer;
  LPixelCount: Integer;
  R, G, B: Byte;
  LSum: Cardinal;
  p: PRColor;
begin
  if (not Assigned(mr)) or
    (mr.width <= 0) or
    (mr.height <= 0) then
    begin
      Exit;
    end;

  for i := 0 to $FF do
    begin
      LHistogram[i] := 0;
      LHistogram[i] := 0;
      LHistogram[i] := 0;
    end;

  LPixelCount := mr.width * mr.height;

  // calculating histogram
  mr.ReadyBits();
  p := @mr.FBits^[0];

  for i := 1 to LPixelCount do
    begin
      R := p^ shr 16 and $FF;
      G := p^ shr 8 and $FF;
      B := p^ and $FF;

      LHistogram[R] := LHistogram[R] + 1;
      LHistogram[G] := LHistogram[G] + 1;
      LHistogram[B] := LHistogram[B] + 1;

      inc(p);
    end;

  // calculating the map
  LSum := 0;

  for i := 0 to $FF do
    begin
      LSum := LSum + LHistogram[i];
      LMap[i] := Round(LSum / (mr.width * mr.height * 3) * $FF);
    end;

  // doing map
  mr.ReadyBits();
  p := @mr.FBits^[0];

  for i := 1 to LPixelCount do
    begin
      R := p^ shr 16 and $FF;
      G := p^ shr 8 and $FF;
      B := p^ and $FF;

      R := LMap[R];
      G := LMap[G];
      B := LMap[B];

      p^ := (p^ and $FF000000) or (R shl 16) or (G shl 8) or B;

      inc(p);
    end;
end;

procedure HistogramEqualize(const mr1, mr2: TMemoryRaster);
var
  LHistogram1: array [0 .. $FF] of Cardinal;
  LHistogram2: array [0 .. $FF] of Cardinal;
  LMap1: array [0 .. $FF] of Byte;
  LMap2: array [0 .. $FF] of Byte;
  i: Integer;
  LPixelCount: Integer;
  R, G, B: Byte;
  LSum1, LSum2: Cardinal;
  p: PRColor;
begin
  if (not Assigned(mr1)) or
    (mr1.width <= 0) or
    (mr1.height <= 0) then
    begin
      Exit;
    end;

  // calculating histogram
  for i := 0 to $FF do
    begin
      LHistogram1[i] := 0;
      LHistogram1[i] := 0;
      LHistogram1[i] := 0;

      LHistogram2[i] := 0;
      LHistogram2[i] := 0;
      LHistogram2[i] := 0;
    end;

  mr1.ReadyBits();
  p := @mr1.FBits^[0];
  LPixelCount := mr1.width * mr1.height;

  for i := 1 to LPixelCount do
    begin
      R := p^ shr 16 and $FF;
      G := p^ shr 8 and $FF;
      B := p^ and $FF;
      LHistogram1[R] := LHistogram1[R] + 1;
      LHistogram1[G] := LHistogram1[G] + 1;
      LHistogram1[B] := LHistogram1[B] + 1;
      inc(p);
    end;

  mr2.ReadyBits();
  p := @mr2.FBits^[0];
  LPixelCount := mr2.width * mr2.height;

  for i := 1 to LPixelCount do
    begin
      R := p^ shr 16 and $FF;
      G := p^ shr 8 and $FF;
      B := p^ and $FF;
      LHistogram2[R] := LHistogram2[R] + 1;
      LHistogram2[G] := LHistogram2[G] + 1;
      LHistogram2[B] := LHistogram2[B] + 1;
      inc(p);
    end;

  // calculating the map
  LSum1 := 0;
  LSum2 := 0;
  for i := 0 to $FF do
    begin
      LSum1 := LSum1 + LHistogram1[i];
      LMap1[i] := Round(LSum1 / (mr1.width * mr1.height * 3) * $FF);
      LSum2 := LSum2 + LHistogram1[i];
      LMap2[i] := Round(LSum2 / (mr2.width * mr2.height * 3) * $FF);
    end;

  // doing map
  mr1.ReadyBits();
  p := @mr1.FBits^[0];
  LPixelCount := mr1.width * mr1.height;
  for i := 1 to LPixelCount do
    begin
      R := p^ shr 16 and $FF;
      G := p^ shr 8 and $FF;
      B := p^ and $FF;
      R := umlMin(LMap1[R], LMap2[R]);
      G := umlMin(LMap1[G], LMap2[G]);
      B := umlMin(LMap1[B], LMap2[B]);
      p^ := (p^ and $FF000000) or (R shl 16) or (G shl 8) or B;
      inc(p);
    end;

  mr2.ReadyBits();
  p := @mr2.FBits^[0];
  LPixelCount := mr2.width * mr2.height;
  for i := 1 to LPixelCount do
    begin
      R := p^ shr 16 and $FF;
      G := p^ shr 8 and $FF;
      B := p^ and $FF;
      R := umlMin(LMap1[R], LMap2[R]);
      G := umlMin(LMap1[G], LMap2[G]);
      B := umlMin(LMap1[B], LMap2[B]);
      p^ := (p^ and $FF000000) or (R shl 16) or (G shl 8) or B;
      inc(p);
    end;
end;

procedure RemoveRedEyes(const mr: TMemoryRaster);
var
  X, Y: Integer;
  w, H: Integer;
  pixptr: PRColorEntry;
  nrv, bluf, redq: TGeoFloat;
  powr, powb, powg: TGeoFloat;
begin
  w := mr.width;
  H := mr.height;

  for Y := 0 to (H - 1) do
    begin
      for X := 0 to (w - 1) do
        begin
          pixptr := PRColorEntry(mr.PixelPtr[X, Y]);
          nrv := pixptr^.G + pixptr^.B;

          if nrv < 1 then
              nrv := 1;

          if pixptr^.G > 1 then
              bluf := pixptr^.B / pixptr^.G
          else
              bluf := pixptr^.B;

          bluf := Max(0.5, Min(1.5, Sqrt(bluf)));
          redq := (pixptr^.R / nrv) * bluf;

          if redq > 0.7 then
            begin
              powr := 1.775 - (redq * 0.75 + 0.25);

              if powr < 0 then
                  powr := 0;

              powr := powr * powr;
              powb := 1 - (1 - powr) / 2;
              powg := 1 - (1 - powr) / 4;

              pixptr^.R := Round(powr * pixptr^.R);
              pixptr^.B := Round(powb * pixptr^.B);
              pixptr^.G := Round(powg * pixptr^.G);
            end;
        end;
    end;
end;

procedure Sepia32(const mr: TMemoryRaster; const Depth: Byte);
var
  LDepth2, i: Integer;
  LPixel: PRColorEntry;
begin
  mr.ReadyBits();
  LDepth2 := Depth * 2;
  LPixel := @mr.FBits^[0];

  for i := 0 to (mr.width * mr.height - 1) do
    begin
      // blue component = gray scaled color
      LPixel^.B := (LPixel^.R + LPixel^.G + LPixel^.B) div 3;

      // set red component of sepia color
      LPixel^.R := Byte(LPixel^.B + LDepth2);

      if LPixel^.R < LDepth2 then
          LPixel^.R := $FF;

      // set green component of sepia color
      LPixel^.G := Byte(LPixel^.B + Depth);

      if LPixel^.G < Depth then
          LPixel^.G := $FF;

      inc(LPixel);
    end;
end;

procedure Sharpen(const DestMR: TMemoryRaster; const SharpenMore: Boolean);
const
  MASK_MATRIX: array [0 .. 24] of Integer =
    (1, 1, 1, 1, 1,
    1, 0, 0, 0, 1,
    1, 0, 0, 0, 1,
    1, 0, 0, 0, 1,
    1, 1, 1, 1, 1);

  SHARPEN_MATRIX_1: array [0 .. 9] of Integer = (-1, -2, -1, -2, 28, -2, -1, -2, -1, 16);
  SHARPEN_MATRIX_2: array [0 .. 9] of Integer = (-2, -1, -2, -1, 28, -1, -2, -1, -2, 16);
  SHARPEN_MORE_MATRIX: array [0 .. 9] of Integer = (0, -1, 0, -1, 6, -1, 0, -1, 0, 2);

var
  LMRCopy: TMemoryRaster;
  LSharpenTime: Integer;
  LLastMatrixNumber: Integer;
  i, X, Y, ix, iy, dx: Integer;
  LDiagonal: Integer;
  LDiagonalX: Integer;
  LDiagonalY: Integer;
  AA, RR, GG, BB: Integer;
  A, R, G, B: Byte;
  LMatrix: array [0 .. 24] of Integer;
  LOriginalRow: array of PRColorArray;
  LDestRow: array of PRColorArray;

  procedure LoadSharpenMatrix(AMatrix: array of Integer);
  var
    i, j: Integer;
  begin
    for j := 0 to 24 do
      begin
        LMatrix[j] := 0;
      end;

    i := 0;

    for j := 6 to 8 do
      begin
        LMatrix[j] := AMatrix[i];
        inc(i);
      end;

    for j := 11 to 13 do
      begin
        LMatrix[j] := AMatrix[i];
        inc(i);
      end;

    for j := 16 to 18 do
      begin
        LMatrix[j] := AMatrix[i];
        inc(i);
      end;

    LLastMatrixNumber := AMatrix[9];
  end;

begin
  LSharpenTime := 0;

  if SharpenMore then
      LoadSharpenMatrix(SHARPEN_MORE_MATRIX)
  else
    begin
      inc(LSharpenTime);

      if (LSharpenTime mod 2) = 0 then
        begin
          LoadSharpenMatrix(SHARPEN_MATRIX_1);
        end
      else
        begin
          LoadSharpenMatrix(SHARPEN_MATRIX_2);
        end;
    end;

  { scanlines arrays 3 octets (24 Bits) optimization bitmaps Maximum 2048
    lines get the access port of the dest and the original bitmap }
  SetLength(LOriginalRow, DestMR.height);
  SetLength(LDestRow, DestMR.height);

  LMRCopy := TMemoryRaster.Create;
  try
    LMRCopy.Assign(DestMR);

    for i := 0 to (DestMR.height - 1) do
      begin
        LOriginalRow[i] := LMRCopy.ScanLine[i];
        LDestRow[i] := DestMR.ScanLine[i];
      end;

    if LLastMatrixNumber = 0 then
        LLastMatrixNumber := 1;

    dx := 0;
    for i := 0 to 24 do
      if (LMatrix[i] and MASK_MATRIX[i]) <> 0 then
          inc(dx);

    if dx = 0 then
        LDiagonal := 1
    else
        LDiagonal := 2;

    for Y := 0 to (DestMR.height - 1) do
      begin
        for X := 0 to (DestMR.width - 1) do
          begin
            AA := 0;
            RR := 0;
            GG := 0;
            BB := 0;

            for LDiagonalY := -LDiagonal to LDiagonal do
              begin
                for LDiagonalX := -LDiagonal to LDiagonal do
                  begin
                    iy := Y + LDiagonalY;
                    ix := X + LDiagonalX;

                    { The original routines in the following checking code was
                      if  (iy >= 1) ...
                      and (ix >= 1) ... }
                    if (iy >= 0) and
                      (ix >= 0) and
                      (iy <= (DestMR.height - 1)) and
                      (ix <= (DestMR.width - 1)) then
                      begin
                        A := LOriginalRow[iy]^[ix] shr 24 and $FF;
                        R := LOriginalRow[iy]^[ix] shr 16 and $FF;
                        G := LOriginalRow[iy]^[ix] shr 8 and $FF;
                        B := LOriginalRow[iy]^[ix] and $FF;
                      end
                    else
                      begin
                        A := LOriginalRow[Y]^[X] shr 24 and $FF;
                        R := LOriginalRow[Y]^[X] shr 16 and $FF;
                        G := LOriginalRow[Y]^[X] shr 8 and $FF;
                        B := LOriginalRow[Y]^[X] and $FF;
                      end;

                    i := 12 + LDiagonalY * 5 + LDiagonalX;
                    AA := AA + A * LMatrix[i];
                    RR := RR + R * LMatrix[i];
                    GG := GG + G * LMatrix[i];
                    BB := BB + B * LMatrix[i];
                  end;
              end;

            AA := AA div LLastMatrixNumber;
            RR := RR div LLastMatrixNumber;
            GG := GG div LLastMatrixNumber;
            BB := BB div LLastMatrixNumber;

            A := ClampInt(AA, 0, $FF);
            R := ClampInt(RR, 0, $FF);
            G := ClampInt(GG, 0, $FF);
            B := ClampInt(BB, 0, $FF);

            LDestRow[Y]^[X] := (A shl 24) or (R shl 16) or (G shl 8) or B;
          end;
      end;
  finally
    DisposeObject(LMRCopy);
    SetLength(LDestRow, 0);
    SetLength(LOriginalRow, 0);
  end;
end;

{$IFDEF RangeCheck}{$R-}{$ENDIF}
{$IFDEF OverflowCheck}{$Q-}{$ENDIF}


procedure AddColorNoise32(const Source: TMemoryRaster; const Amount_: Integer);
var
  i, LAmount: Integer;
  RR, GG, BB, A, R, G, B: Cardinal;
  p: PRColor;
  rnd: TMT19937Random;
begin
  LAmount := Amount_;

  if LAmount < 0 then
    begin
      LAmount := 0;
    end;

  p := @Source.Bits^[0];
  rnd := TMT19937Random.Create;

  for i := 0 to (Source.width * Source.height - 1) do
    begin
      A := p^ and $FF000000;

      if A > 0 then
        begin
          RR := p^ shr 16 and $FF;
          GG := p^ shr 8 and $FF;
          BB := p^ and $FF;

          RR := RR + (rnd.Rand32(LAmount) - (LAmount shr 1));
          GG := GG + (rnd.Rand32(LAmount) - (LAmount shr 1));
          BB := BB + (rnd.Rand32(LAmount) - (LAmount shr 1));

          R := IfThen(RR > $FF, $FF, RR);
          G := IfThen(GG > $FF, $FF, RR);
          B := IfThen(BB > $FF, $FF, RR);
          p^ := A or (R shl 16) or (G shl 8) or B;
        end;

      inc(p);
    end;

  DisposeObject(rnd);
end;

procedure AddMonoNoise32(const Source: TMemoryRaster; const Amount_: Integer);
var
  i, LDelta, LAmount: Integer;
  RR, GG, BB, A, R, G, B: Cardinal;
  p: PRColor;
  rnd: TMT19937Random;
begin
  LAmount := Amount_;

  if LAmount < 0 then
    begin
      LAmount := 0;
    end;

  p := @Source.Bits^[0];
  rnd := TMT19937Random.Create;

  for i := 0 to (Source.width * Source.height - 1) do
    begin
      A := p^ and $FF000000;

      if A > 0 then
        begin
          LDelta := rnd.Rand32(LAmount) - (LAmount shr 1);

          RR := p^ shr 16 and $FF;
          GG := p^ shr 8 and $FF;
          BB := p^ and $FF;

          RR := RR + LDelta;
          GG := GG + LDelta;
          BB := BB + LDelta;

          R := IfThen(RR > $FF, $FF, RR);
          G := IfThen(GG > $FF, $FF, RR);
          B := IfThen(BB > $FF, $FF, RR);
          p^ := A or (R shl 16) or (G shl 8) or B;
        end;

      inc(p);
    end;
  DisposeObject(rnd);
end;
{$IFDEF RangeCheck}{$R+}{$ENDIF}
{$IFDEF OverflowCheck}{$Q+}{$ENDIF}


procedure Diagonal(const Source, dest: TMemoryRaster; const BackColor: TRColor; const Amount: Integer; const DiagDirection: TDiagonalDirection);
var
  LSourceRow, LDestRow: PRColorArray;
  i, j, LDelta, LHeight, LWidth: Integer;
  LSourceColumn, LDestColumn: Integer;
  LFactor: Extended;
begin
  LSourceColumn := 0;
  LDestColumn := 0;

  if (Amount >= 0) and (Amount <= 100) then
    begin
      LHeight := Source.height;
      LWidth := Source.width;

      dest.SetSize(LWidth, LHeight, BackColor);

      LFactor := LWidth / (LWidth + (Amount / 100) * LHeight);

      for j := 0 to (LHeight - 1) do
        begin
          LSourceRow := Source.ScanLine[j];
          LDestRow := dest.ScanLine[j];
          LDelta := Round(Amount / 100 * j);

          for i := 0 to (LWidth - 1) do
            begin
              case DiagDirection of
                ddLeftDiag:
                  begin
                    LSourceColumn := i;
                    LDestColumn := Round(LFactor * (i + LDelta));
                  end;

                ddRightDiag:
                  begin
                    LSourceColumn := (LWidth - 1 - i);
                    LDestColumn := Round(LWidth - 1 - LFactor * (i + LDelta));
                  end;
              end;

              LDestRow^[LDestColumn] := LSourceRow^[LSourceColumn];
            end;
        end;
    end;
end;

procedure CheckParams(Src, Dst: TMemoryRaster; ResizeDst: Boolean = True);
const
  SEmptySource = 'The source is nil';
  SEmptyDestination = 'Destination is nil';
begin
  if not Assigned(Src) then
      RaiseInfo(SEmptySource);

  if not Assigned(Dst) then
      RaiseInfo(SEmptyDestination);

  if ResizeDst then
      Dst.SetSize(Src.width, Src.height);
end;

procedure GrayscaleToAlpha(Src: TMemoryRaster);
var
  i: Integer;
  C: PRColorEntry;
begin
  Src.ReadyBits();
  for i := (Src.width * Src.height) - 1 downto 0 do
    begin
      C := @Src.FBits^[i];
      C^.A := RColor2Gray(C^.BGRA);
    end;
end;

procedure AlphaToGrayscale(Src: TMemoryRaster);
var
  i: Integer;
  C: PRColorEntry;
begin
  Src.ReadyBits();
  for i := (Src.width * Src.height) - 1 downto 0 do
    begin
      C := @Src.FBits^[i];
      C^.R := C^.A;
      C^.G := C^.A;
      C^.B := C^.A;
    end;
end;

procedure IntensityToAlpha(Src: TMemoryRaster);
var
  i: Integer;
  C: PRColorEntry;
  f: TGeoFloat;
begin
  Src.ReadyBits();
  for i := (Src.width * Src.height) - 1 downto 0 do
    begin
      C := @Src.FBits^[i];
      C^.A := ((C^.R * 61 + C^.G * 174 + C^.B * 21) shr 8);
    end;
end;

procedure ReversalAlpha(Src: TMemoryRaster);
var
  i: Integer;
  C: PRColorEntry;
begin
  Src.ReadyBits();
  for i := (Src.width * Src.height) - 1 downto 0 do
    begin
      C := @Src.FBits^[i];
      C^.A := $FF - C^.A;
    end;
end;

procedure RGBToGrayscale(Src: TMemoryRaster);
var
  i: Integer;
  C: PRColorEntry;
begin
  Src.ReadyBits();
  for i := (Src.width * Src.height) - 1 downto 0 do
    begin
      C := @Src.FBits^[i];
      C^.R := RColor2Gray(C^.BGRA);
      C^.G := C^.R;
      C^.B := C^.R;
    end;
end;

procedure FillBlackGrayBackgroundTexture(bk: TMemoryRaster; block_size: Integer; bkColor, color1, color2: TRColor);
var
  i, j: Integer;
  fi, fj: Boolean;
begin
  bk.Clear(bkColor);
  fi := True;
  i := 0;
  while i < bk.width do
    begin
      fi := not fi;
      fj := fi;
      j := 0;
      while j < bk.height do
        begin
          bk.FillRect(i, j, i + block_size - 1, j + block_size - 1, if_(fj, color1, color2));
          fj := not fj;
          inc(j, block_size);
        end;
      inc(i, block_size);
    end;
end;

procedure FillBlackGrayBackgroundTexture(bk: TMemoryRaster; block_size: Integer);
begin
  FillBlackGrayBackgroundTexture(bk, block_size, RColorF(0.15,0.15,0.15), RColorF(0.25,0.25,0.25), RColorF(0.2,0.2,0.2));
end;

procedure ColorToTransparent(SrcColor: TRColor; Src, Dst: TMemoryRaster);
var
  i, j: Integer;
  C: TRColorEntry;
begin
  CheckParams(Src, Dst);
  for i := 0 to Src.width - 1 do
    for j := 0 to Src.height - 1 do
      begin
        C.BGRA := Src[i, j];
        if C.BGRA = SrcColor then
            Dst[i, j] := RColor(0, 0, 0, 0)
        else
            Dst[i, j] := C.BGRA;
      end;
end;

function BuildSequenceFrame(bmp32List: TCoreClassListForObj; Column: Integer; Transparent: Boolean): TSequenceMemoryRaster;
var
  C: TRColor;
  bmp: TMemoryRaster;
  AMaxWidth, AMaxHeight: Integer;
  i: Integer;
  idx, X, Y: Integer;
  newbmp: TMemoryRaster;
  rowcnt: Integer;
begin
  if Column > bmp32List.Count then
      Column := bmp32List.Count;

  AMaxWidth := 0;
  AMaxHeight := 0;
  for i := 0 to bmp32List.Count - 1 do
    begin
      bmp := bmp32List[i] as TMemoryRaster;
      if Transparent then
          bmp.ColorTransparent(bmp[0, 0]);

      if bmp.width > AMaxWidth then
          AMaxWidth := bmp.width;
      if bmp.height > AMaxHeight then
          AMaxHeight := bmp.height;
    end;

  Result := TSequenceMemoryRaster.Create;

  rowcnt := bmp32List.Count div Column;
  if bmp32List.Count mod Column > 0 then
      inc(rowcnt);

  if Transparent then
      C := RColor(0, 0, 0, 0)
  else
      C := RColor(0, 0, 0, $FF);

  Result.SetSize(AMaxWidth * Column, AMaxHeight * rowcnt, C);

  idx := 0;
  X := 0;
  Y := 0;

  for i := 0 to bmp32List.Count - 1 do
    begin
      bmp := bmp32List[i] as TMemoryRaster;
      if (bmp.width <> AMaxWidth) or (bmp.height <> AMaxHeight) then
        begin
          newbmp := TMemoryRaster.Create;
          newbmp.ZoomFrom(bmp, AMaxWidth, AMaxHeight);
          BlockTransfer(Result, X, Y, Result.BoundsRect, newbmp, newbmp.BoundsRect, dmOpaque);
          DisposeObject(newbmp);
        end
      else
        begin
          BlockTransfer(Result, X, Y, Result.BoundsRect, bmp, bmp.BoundsRect, dmOpaque);
        end;

      if idx + 1 >= Column then
        begin
          idx := 0;
          X := 0;
          Y := Y + AMaxHeight;
        end
      else
        begin
          inc(idx);
          X := X + AMaxWidth;
        end;
    end;

  Result.Total := bmp32List.Count;
  Result.Column := Column;
end;

function GetSequenceFrameRect(bmp: TMemoryRaster; Total, Column, index: Integer): TRect;
var
  rowIdx, colIdx: Integer;
  Row: Integer;
  Width_, Height_: Integer;
begin
  if Total <= 1 then
      Exit(bmp.BoundsRect);
  if Column > Total then
      Column := Total;

  if index > Total - 1 then
      index := Total - 1;
  if index < 0 then
      index := 0;

  colIdx := index mod Column;
  rowIdx := index div Column;
  Row := Total div Column;
  if Total mod Column > 0 then
      inc(Row);

  Width_ := bmp.width div Column;
  Height_ := bmp.height div Row;

  Result := Rect(colIdx * Width_, rowIdx * Height_, (colIdx + 1) * Width_, (rowIdx + 1) * Height_);
end;

procedure GetSequenceFrameOutput(bmp: TMemoryRaster; Total, Column, index: Integer; output: TMemoryRaster);
var
  R: TRect;
  w, H: Integer;
begin
  R := GetSequenceFrameRect(bmp, Total, Column, index);
  w := R.Right - R.Left;
  H := R.Bottom - R.Top;
  output.SetSize(w, H);
  BlockTransfer(output, 0, 0, output.BoundsRect, bmp, R, dmOpaque);
end;

function AnalysisColors(mr: TMemoryRaster; ignoreColors: TRColors; MaxCount: Integer): TRColors;
var
  siz: Integer;
  p: PRColor;
begin
  Result := TRColors.Create;
  siz := mr.MemorySize;
  if siz <= 0 then
      Exit;

  p := @mr.Bits^[0];
  while siz > 0 do
    begin
      if ((ignoreColors = nil) or (ignoreColors.IndexOf(p^) < 0)) and (Result.IndexOf(p^) < 0) then
        begin
          Result.Add(p^);
          if Result.Count > MaxCount then
              break;
        end;
      inc(p);
      dec(siz, SizeOf(TRColor));
    end;
end;

function BlendReg(f, B: TRColor): TRColor;
var
  FX: TRColorEntry absolute f;
  BX: TRColorEntry absolute B;
  AF, AB: PByteArray;
  FA: Byte;
begin
  FA := FX.A;

  if FA = 0 then
    begin
      Result := B;
      Exit;
    end;

  if FA = $FF then
    begin
      Result := f;
      Exit;
    end;

  with BX do
    begin
      AF := @DivTable[FA];
      AB := @DivTable[not FA];
      R := AF^[FX.R] + AB^[R];
      G := AF^[FX.G] + AB^[G];
      B := AF^[FX.B] + AB^[B];
    end;
  Result := B;
end;

procedure BlendMem(f: TRColor; var B: TRColor);
var
  FX: TRColorEntry absolute f;
  BX: TRColorEntry absolute B;
  AF, AB: PByteArray;
  FA: Byte;
begin
  FA := FX.A;

  if FA = 0 then
      Exit;

  if FA = $FF then
    begin
      B := f;
      Exit;
    end;

  with BX do
    begin
      AF := @DivTable[FA];
      AB := @DivTable[not FA];
      R := AF^[FX.R] + AB^[R];
      G := AF^[FX.G] + AB^[G];
      B := AF^[FX.B] + AB^[B];
    end;
end;

function BlendRegEx(f, B, M: TRColor): TRColor;
var
  FX: TRColorEntry absolute f;
  BX: TRColorEntry absolute B;
  AF, AB: PByteArray;
begin
  AF := @DivTable[M];

  M := AF^[FX.A];

  if M = 0 then
    begin
      Result := B;
      Exit;
    end;

  if M = $FF then
    begin
      Result := f;
      Exit;
    end;

  with BX do
    begin
      AF := @DivTable[M];
      AB := @DivTable[$FF - M];
      R := AF^[FX.R] + AB^[R];
      G := AF^[FX.G] + AB^[G];
      B := AF^[FX.B] + AB^[B];
    end;
  Result := B;
end;

procedure BlendMemEx(const f: TRColor; var B: TRColor; M: TRColor);
var
  FX: TRColorEntry absolute f;
  BX: TRColorEntry absolute B;
  AF, AB: PByteArray;
begin
  AF := @DivTable[M];

  M := AF^[FX.A];

  if M = 0 then
    begin
      Exit;
    end;

  if M >= $FF then
    begin
      B := f;
      Exit;
    end;

  with BX do
    begin
      AF := @DivTable[M];
      AB := @DivTable[$FF - M];
      R := AF^[FX.R] + AB^[R];
      G := AF^[FX.G] + AB^[G];
      B := AF^[FX.B] + AB^[B];
    end;
end;

procedure BlendLine(Src, Dst: PRColor; Count: Integer);
begin
  while Count > 0 do
    begin
      BlendMem(Src^, Dst^);
      inc(Src);
      inc(Dst);
      dec(Count);
    end;
end;

procedure BlendLineEx(Src, Dst: PRColor; Count: Integer; M: TRColor);
begin
  while Count > 0 do
    begin
      BlendMemEx(Src^, Dst^, M);
      inc(Src);
      inc(Dst);
      dec(Count);
    end;
end;

function CombineReg(X, Y, w: TRColor): TRColor;
var
  XE: TRColorEntry absolute X;
  YE: TRColorEntry absolute Y;
  AF, AB: PByteArray;
begin
  if w = 0 then
    begin
      Result := Y;
      Exit;
    end;

  if w >= $FF then
    begin
      Result := X;
      Exit;
    end;

  with XE do
    begin
      AF := @DivTable[w];
      AB := @DivTable[$FF - w];
      R := AB^[YE.R] + AF^[R];
      G := AB^[YE.G] + AF^[G];
      B := AB^[YE.B] + AF^[B];
    end;
  Result := X;
end;

procedure CombineMem(X: TRColor; var Y: TRColor; w: TRColor);
var
  XE: TRColorEntry absolute X;
  YE: TRColorEntry absolute Y;
  AF, AB: PByteArray;
begin
  if w = 0 then
    begin
      Exit;
    end;

  if w >= $FF then
    begin
      Y := X;
      Exit;
    end;

  with XE do
    begin
      AF := @DivTable[w];
      AB := @DivTable[$FF - w];
      R := AB^[YE.R] + AF^[R];
      G := AB^[YE.G] + AF^[G];
      B := AB^[YE.B] + AF^[B];
    end;
  Y := X;
end;

procedure CombineLine(Src, Dst: PRColor; Count: Integer; w: TRColor);
begin
  while Count > 0 do
    begin
      CombineMem(Src^, Dst^, w);
      inc(Src);
      inc(Dst);
      dec(Count);
    end;
end;

function MergeReg(f, B: TRColor): TRColor;
var
  FA, BA, WA: TRColor;
  fw, BW: PByteArray;
  FX: TRColorEntry absolute f;
  BX: TRColorEntry absolute B;
  RX: TRColorEntry absolute Result;
begin
  FA := f shr 24;
  BA := B shr 24;
  if FA = $FF then
      Result := f
  else if FA = $0 then
      Result := B
  else if BA = $0 then
      Result := f
  else
    begin
      RX.A := DivTable[FA xor $FF, BA xor $FF] xor $FF;
      WA := RcTable[RX.A, FA];
      fw := @DivTable[WA];
      BW := @DivTable[WA xor $FF];
      RX.R := fw^[FX.R] + BW^[BX.R];
      RX.G := fw^[FX.G] + BW^[BX.G];
      RX.B := fw^[FX.B] + BW^[BX.B];
    end;
end;

function MergeRegEx(f, B, M: TRColor): TRColor;
begin
  Result := MergeReg(DivTable[M, f shr 24] shl 24 or f and $00FFFFFF, B);
end;

procedure MergeMem(f: TRColor; var B: TRColor);
begin
  B := MergeReg(f, B);
end;

procedure MergeMemEx(f: TRColor; var B: TRColor; M: TRColor);
begin
  B := MergeReg(DivTable[M, f shr 24] shl 24 or f and $00FFFFFF, B);
end;

procedure MergeLine(Src, Dst: PRColor; Count: Integer);
begin
  while Count > 0 do
    begin
      Dst^ := MergeReg(Src^, Dst^);
      inc(Src);
      inc(Dst);
      dec(Count);
    end;
end;

procedure MergeLineEx(Src, Dst: PRColor; Count: Integer; M: TRColor);
var
  PM: PByteArray absolute M;
begin
  PM := @DivTable[M];
  while Count > 0 do
    begin
      Dst^ := MergeReg((PM^[Src^ shr 24] shl 24) or (Src^ and $00FFFFFF), Dst^);
      inc(Src);
      inc(Dst);
      dec(Count);
    end;
end;

procedure jls_RasterToRaw3(ARaster: TMemoryRaster; RawStream: TCoreClassStream);
var
  i, j, n: Integer;
  Buf: array of Byte;
  pce: TRColorEntry;
begin
  SetLength(Buf, ARaster.width * 3);

  for i := 0 to ARaster.height - 1 do
    begin
      n := 0;
      for j := 0 to ARaster.width - 1 do
        begin
          pce := TRColorEntry(ARaster.Pixel[j, i]);
          Buf[n] := pce.R;
          Buf[n + 1] := pce.G;
          Buf[n + 2] := pce.B;
          inc(n, 3);
        end;
      RawStream.write(Buf[0], ARaster.width * 3)
    end;
  RawStream.Position := 0;
  SetLength(Buf, 0);
end;

procedure jls_RasterToRaw1(ARaster: TMemoryRaster; RawStream: TCoreClassStream);
var
  i, j: Integer;
  Buf: array of Byte;
begin
  SetLength(Buf, ARaster.width);

  for i := 0 to ARaster.height - 1 do
    begin
      for j := 0 to ARaster.width - 1 do
          Buf[j] := ARaster.PixelGray[j, i];
      RawStream.write(Buf[0], ARaster.width)
    end;
  RawStream.Position := 0;
  SetLength(Buf, 0);
end;

procedure jls_GrayRasterToRaw1(const ARaster: PByteRaster; RawStream: TCoreClassStream);
var
  i, j: Integer;
begin
  for i := 0 to length(ARaster^) - 1 do
      RawStream.write(ARaster^[i][0], length(ARaster^[i]));
  RawStream.Position := 0;
end;

function EncodeJpegLSRasterToStream3(ARaster: TMemoryRaster; const stream: TCoreClassStream): Boolean;
var
  LInput: TMemoryStream64;
  Info: TJlsParameters;
begin
  LInput := TMemoryStream64.Create;
  LInput.Size := ARaster.width * ARaster.height * 3;
  LInput.Position := 0;
  FillPtrByte(@Info, SizeOf(Info), 0);

  try
    jls_RasterToRaw3(ARaster, LInput);
    Info.width := ARaster.width;
    Info.height := ARaster.height;
    Info.BitsPerSample := 8;
    Info.Components := 3;
    Info.Custom.t1 := 3;
    Info.Custom.t2 := 7;
    Info.Custom.t3 := 21;
    Info.Custom.Reset := 64;
    Info.AllowedLossyError := 0;

    Result := jpegls_compress(LInput, stream, @Info);
  finally
      LInput.Free;
  end;
end;

function EncodeJpegLSRasterToStream1(ARaster: TMemoryRaster; const stream: TCoreClassStream): Boolean;
var
  LInput: TMemoryStream64;
  Info: TJlsParameters;
begin
  LInput := TMemoryStream64.Create;
  FillPtrByte(@Info, SizeOf(Info), 0);

  try
    jls_RasterToRaw1(ARaster, LInput);
    Info.width := ARaster.width;
    Info.height := ARaster.height;
    Info.BitsPerSample := 8;
    Info.Components := 1;
    Info.Custom.t1 := 3;
    Info.Custom.t2 := 7;
    Info.Custom.t3 := 21;
    Info.Custom.Reset := 64;
    Info.AllowedLossyError := 0;

    Result := jpegls_compress(LInput, stream, @Info);
  finally
      LInput.Free;
  end;
end;

procedure jls_RawToRaster(const AStream: TMemoryStream64; var Info: TJlsParameters; const output: TMemoryRaster);
var
  j, i: Integer;
  Src: PByte;
  srcword: PWORD;
  R, G, B, A: Byte;
begin
  case Info.Components of
    1: case Info.BitsPerSample of
        8:
          begin
            output.SetSize(Info.width, Info.height);

            Src := AStream.Memory;
            for j := 0 to output.height - 1 do
              for i := 0 to Info.width - 1 do
                begin
                  output.PixelGray[i, j] := Src^;
                  inc(Src);
                end;
          end;
        10, 12, 15:
          begin
            output.SetSize(Info.width, Info.height);
            srcword := AStream.Memory;

            for j := 0 to output.height - 1 do
              for i := 0 to Info.width - 1 do
                begin
                  output.PixelGray[i, j] := srcword^;
                  inc(srcword);
                end;
          end;
        16:
          begin
            output.SetSize(Info.width, Info.height);
            srcword := AStream.Memory;

            for j := 0 to output.height - 1 do
              for i := 0 to Info.width - 1 do
                begin
                  R := Word(((srcword^ and $F800) shr 8)); // to rgb888
                  G := Word(((srcword^ and $07E0) shr 3));
                  B := Word(((srcword^ and $001F) shl 3));
                  output.PixelGray[i, j] := ((R shl 1) + (G shl 2) + G + B) shr 3;
                  inc(srcword);
                end;
          end;
        else
          RaiseInfo('decode error');
      end;
    3:
      if Info.BitsPerSample = 8 then
        begin
          output.SetSize(Info.width, Info.height);

          Src := AStream.Memory;
          for j := 0 to output.height - 1 do
            for i := 0 to Info.width - 1 do
              begin
                R := Src^;
                inc(Src);
                G := Src^;
                inc(Src);
                B := Src^;
                inc(Src);
                output.Pixel[i, j] := RColor(R, G, B, $FF);
              end;
        end
      else
          RaiseInfo('decode error');
  end;
end;

function DecodeJpegLSRasterFromStream(const stream: TCoreClassStream; ARaster: TMemoryRaster): Boolean;
var
  LOutput: TMemoryStream64;
  Info: TJlsParameters;
begin
  LOutput := TMemoryStream64.Create;
  FillPtrByte(@Info, SizeOf(Info), 0);
  try
    Result := jpegls_decompress(stream, LOutput, @Info);

    if Result then
        jls_RawToRaster(LOutput, Info, ARaster);
  finally
      LOutput.Free;
  end;
end;

function EncodeJpegLSGrayRasterToStream(const ARaster: PByteRaster; const stream: TCoreClassStream): Boolean;
var
  LInput: TMemoryStream64;
  Info: TJlsParameters;
begin
  LInput := TMemoryStream64.Create;
  FillPtrByte(@Info, SizeOf(Info), 0);

  try
    jls_GrayRasterToRaw1(ARaster, LInput);
    Info.width := length(ARaster^[0]);
    Info.height := length(ARaster^);
    Info.BitsPerSample := 8;
    Info.Components := 1;
    Info.Custom.t1 := 3;
    Info.Custom.t2 := 7;
    Info.Custom.t3 := 21;
    Info.Custom.Reset := 64;
    Info.AllowedLossyError := 0;

    Result := jpegls_compress(LInput, stream, @Info);
  finally
      LInput.Free;
  end;
end;

function DecodeJpegLSGrayRasterFromStream(const stream: TCoreClassStream; var ARaster: TByteRaster): Boolean;
var
  LOutput: TMemoryStream64;
  Info: TJlsParameters;
  j, i: Integer;
  Src: PByte;
begin
  Result := False;
  LOutput := TMemoryStream64.Create;
  FillPtrByte(@Info, SizeOf(Info), 0);
  try
    if jpegls_decompress(stream, LOutput, @Info) and (Info.Components = 1) and (Info.BitsPerSample = 8) then
      begin
        SetLength(ARaster, Info.height, Info.width);
        Src := LOutput.Memory;
        for j := 0 to Info.height - 1 do
          for i := 0 to Info.width - 1 do
            begin
              ARaster[j, i] := Src^;
              inc(Src);
            end;
        Result := True;
      end;
  finally
      LOutput.Free;
  end;
end;

type
  THoughLineProcessor = record
    AlphaStart, AlphaStep, MinDist: TGeoFloat;
    AlphaSteps, AccumulatorSize, AccumulatedCounts: Integer;
    HoughAccumulator: array of Integer;
    PageWidth, PageHeight: Integer;
    box: TRect;

    // compute final angle for given angle step.
    function GetFinalAngle(StepIndex: Integer): TGeoFloat;
    // compute angle and distance parameters for all lines
    // going through point [X, Y].
    procedure computeLines(X, Y: Integer);
    // Chooses "best" lines (with the most votes) from the accumulator
    function GetBestLines(Count: Integer): THoughLineArry;
  end;

function THoughLineProcessor.GetFinalAngle(StepIndex: Integer): TGeoFloat;
begin
  Result := AlphaStart + StepIndex * AlphaStep;
end;

procedure THoughLineProcessor.computeLines(X, Y: Integer);
var
  d, Rads: TGeoFloat;
  i, DIndex, index: Integer;
  Sin_, Cos_: TGeoFloat;
begin
  for i := 0 to AlphaSteps - 1 do
    begin
      // Angle for current step in radians
      Rads := GetFinalAngle(i) * PI / 180;
      SinCos(Rads, Sin_, Cos_);
      // Parameter D(distance from origin) of the line y=tg(alpha)x + d
      d := Y * Cos_ - X * Sin_;
      // compute index into accumulator for current line
      DIndex := Abs(Trunc(d - MinDist));
      Index := DIndex * AlphaSteps + i;
      // Add one vote for current line
      HoughAccumulator[Index] := HoughAccumulator[Index] + 1;
    end;
end;

// Chooses "best" lines (with the most votes) from the accumulator
function THoughLineProcessor.GetBestLines(Count: Integer): THoughLineArry;
var
  i, j, DistIndex, AlphaIndex: Integer;
  Temp: THoughLine;
begin
  AccumulatedCounts := 0;
  SetLength(Result, Count);

  for i := 0 to AccumulatorSize - 1 do
    begin
      if HoughAccumulator[i] > Result[Count - 1].Count then
        begin
          // Current line has more votes than the last selected one,
          // let's put it the pot
          Result[Count - 1].Count := HoughAccumulator[i];
          Result[Count - 1].index := i;
          j := Count - 1;

          // Sort the lines based on number of votes
          while (j > 0) and (Result[j].Count > Result[j - 1].Count) do
            begin
              Temp := Result[j];
              Result[j] := Result[j - 1];
              Result[j - 1] := Temp;
              j := j - 1;
            end;
        end;

      AccumulatedCounts := AccumulatedCounts + HoughAccumulator[i];
    end;

  for i := 0 to Count - 1 do
    begin
      // Caculate line angle and distance according to index in the accumulator
      DistIndex := Result[i].index div AlphaSteps;
      AlphaIndex := Result[i].index - DistIndex * AlphaSteps;
      Result[i].alpha := GetFinalAngle(AlphaIndex);
      Result[i].Distance := DistIndex + MinDist;
    end;
end;

function BuildRasterHoughLine(const MaxAngle_, AlphaStep_: TGeoFloat; const BestLinesCount_: Integer; raster_: TMemoryRaster): THoughLineArry;
var
  morph_: TMorphMath;
  binTmp: TMorphBin;
begin
  with raster_.FitScaleAsNew(1024, 1024) do
    begin
      morph_ := BuildMorphomatics(TMorphPixel.mpYIQ_Y);
      morph_.Invert;
      morph_.Sobel(False);
      binTmp := TMorphBin.Create;
      binTmp.SetSize(11, 3, True);
      morph_.Closing(binTmp);
      DisposeObject(binTmp);
      Result := BuildMorphHoughLine(MaxAngle_, AlphaStep_, 0.5, BestLinesCount_, morph_);
      DisposeObject(morph_);
      Free;
    end;
end;

function BuildMorphHoughLine(const MaxAngle_, AlphaStep_, Treshold_: TGeoFloat; const BestLinesCount_: Integer; morph_: TMorphMath): THoughLineArry;
var
  Processor: THoughLineProcessor;
  i, j: Integer;
begin
  with Processor do
    begin
      AlphaStep := AlphaStep_;
      box := Rect(0, 0, morph_.width - 1, morph_.height - 1);
      PageWidth := box.Right - box.Left;
      PageHeight := box.Bottom - box.Top;
      if (box.Bottom = morph_.height - 1) then
          dec(PageHeight);
      AlphaStart := -MaxAngle_;
      AlphaSteps := Ceil(2 * MaxAngle_ / AlphaStep);
      MinDist := -Max(PageWidth, PageHeight);
      AccumulatorSize := (2 * (PageWidth + PageHeight)) * AlphaSteps;
      SetLength(HoughAccumulator, AccumulatorSize);
      // computeulate Hough transform
      for j := 0 to PageHeight - 1 do
        for i := 0 to PageWidth - 1 do
          if (morph_[box.Left + i, box.Top + j] < Treshold_) and (morph_[box.Left + i, box.Top + j + 1] >= Treshold_) then
              computeLines(i, j);
      // Get the best lines with most votes
      Result := GetBestLines(BestLinesCount_);
      SetLength(HoughAccumulator, 0);
    end;
end;

function BuildBinHoughLine(const MaxAngle_, AlphaStep_: TGeoFloat; const BestLinesCount_: Integer; bin_: TMorphBin): THoughLineArry;
var
  Processor: THoughLineProcessor;
  i, j: Integer;
begin
  with Processor do
    begin
      AlphaStep := AlphaStep_;
      box := Rect(0, 0, bin_.width - 1, bin_.height - 1);
      PageWidth := box.Right - box.Left;
      PageHeight := box.Bottom - box.Top;
      if (box.Bottom = bin_.height - 1) then
          dec(PageHeight);
      AlphaStart := -MaxAngle_;
      AlphaSteps := Ceil(2 * MaxAngle_ / AlphaStep);
      MinDist := -Max(PageWidth, PageHeight);
      AccumulatorSize := (2 * (PageWidth + PageHeight)) * AlphaSteps;
      SetLength(HoughAccumulator, AccumulatorSize);
      // computeulate Hough transform
      for j := 0 to PageHeight - 1 do
        for i := 0 to PageWidth - 1 do
          if (bin_[box.Left + i, box.Top + j]) and (not bin_[box.Left + i, box.Top + j + 1]) then
              computeLines(i, j);
      // Get the best lines with most votes
      Result := GetBestLines(BestLinesCount_);
      SetLength(HoughAccumulator, 0);
    end;
end;

function DocumentRotationDetected_MaxMatched(var BestLines: THoughLineArry): TGeoFloat;
  function GetMaxCountLineIndex(): Integer;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 0 to length(BestLines) - 1 do
      if BestLines[i].Count > BestLines[Result].Count then
          Result := i;
  end;

begin
  // max matched of the selected lines to get the rotation angle of the image
  Result := NormalizeDegAngle(BestLines[GetMaxCountLineIndex()].alpha);
end;

function DocumentRotationDetected_MaxDistance(var BestLines: THoughLineArry): TGeoFloat;
  function GetMaxDistLineIndex(): Integer;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 0 to length(BestLines) - 1 do
      if BestLines[i].Distance > BestLines[Result].Distance then
          Result := i;
  end;

begin
  // max distance of the selected lines to get the rotation angle of the image
  Result := NormalizeDegAngle(BestLines[GetMaxDistLineIndex()].alpha);
end;

function DocumentRotationDetected_AVG(var BestLines: THoughLineArry): TGeoFloat;
var
  SumAngles: TGeoFloat;
  i: Integer;
begin
  // Average angles of the selected lines to get the rotation angle of the image
  SumAngles := 0;
  for i := 0 to length(BestLines) - 1 do
      SumAngles := SumAngles + BestLines[i].alpha;

  Result := NormalizeDegAngle(SumAngles / length(BestLines));
end;

procedure YV12ToRasterization(sour: TCoreClassStream; dest: TMemoryRaster);
var
  head: TYV12Head;
  nStream: TMemoryStream64;
  needFree: Boolean;
  memsiz: NativeUInt;
  luma_ptr, u_ptr, v_ptr: PByte;
  stride, stride_c: Integer;
begin
  if sour.read(head, SizeOf(head)) <> SizeOf(head) then
      Exit;

  if head.Version <> 10 then
      Exit;

  nStream := nil;
  if Boolean(head.Compessed) then
    begin
      nStream := TMemoryStream64.Create;
      DecompressStream(sour, nStream);
      luma_ptr := nStream.Memory;
      needFree := False;
    end
  else if sour is TMemoryStream64 then
    begin
      luma_ptr := TMemoryStream64(sour).PositionAsPtr;
      needFree := False;
    end
  else
    begin
      memsiz := head.width * head.height + (head.width * head.height) div 2;
      luma_ptr := GetMemory(memsiz);
      sour.read(luma_ptr^, memsiz);
      needFree := True;
    end;

  u_ptr := luma_ptr;
  inc(u_ptr, head.width * head.height);
  v_ptr := u_ptr;
  inc(v_ptr, (head.width * head.height) div 4);
  stride := head.width;
  stride_c := head.width div 2;
  YV12ToRaster(luma_ptr, u_ptr, v_ptr, head.width, head.height, stride, stride_c, dest, False, False);

  if needFree then
      FreeMemory(luma_ptr);

  if Boolean(head.Compessed) then
      DisposeObject(nStream);
end;

procedure RasterizationToYV12(Compressed: Boolean; sour: TMemoryRaster; dest: TCoreClassStream);
var
  head: TYV12Head;
  memsiz: NativeUInt;
  luma_ptr, u_ptr, v_ptr: PByte;
  m64: TMemoryStream64;
begin
  head.Version := 10;
  head.Compessed := Byte(Compressed);
  head.width := (sour.width shr 1) * 2;
  head.height := (sour.height shr 1) * 2;

  memsiz := head.width * head.height + (head.width * head.height) div 2;
  luma_ptr := GetMemory(memsiz);
  u_ptr := luma_ptr;
  inc(u_ptr, head.width * head.height);
  v_ptr := u_ptr;
  inc(v_ptr, (head.width * head.height) div 4);
  RasterToYV12(sour, luma_ptr, u_ptr, v_ptr, head.width, head.height);

  dest.write(head, SizeOf(head));
  if Boolean(head.Compessed) then
    begin
      m64 := TMemoryStream64.Create;
      m64.SetPointerWithProtectedMode(luma_ptr, memsiz);
      FastCompressStream(m64, dest);
      DisposeObject(m64);
    end
  else
    begin
      dest.write(luma_ptr^, memsiz);
    end;
  FreeMemory(luma_ptr);
end;

type
  TOriginSize = packed record
    width, height: Integer;
  end;

procedure HalfYUVToRasterization(sour: TCoreClassStream; dest: TMemoryRaster);
var
  oriSiz: TOriginSize;
begin
  if sour.read(oriSiz, SizeOf(oriSiz)) <> SizeOf(oriSiz) then
      Exit;
  YV12ToRasterization(sour, dest);
  dest.Zoom(oriSiz.width, oriSiz.height);
end;

procedure RasterizationToHalfYUV(Compressed: Boolean; sour: TMemoryRaster; dest: TCoreClassStream);
var
  oriSiz: TOriginSize;
  n: TMemoryRaster;
begin
  oriSiz.width := sour.width;
  oriSiz.height := sour.height;
  dest.write(oriSiz, SizeOf(oriSiz));

  n := TMemoryRaster.Create;
  n.ZoomFrom(sour, sour.width shr 1, sour.height shr 1);
  RasterizationToYV12(Compressed, n, dest);
  DisposeObject(n);
end;

procedure QuartYUVToRasterization(sour: TCoreClassStream; dest: TMemoryRaster);
var
  oriSiz: TOriginSize;
begin
  if sour.read(oriSiz, SizeOf(oriSiz)) <> SizeOf(oriSiz) then
      Exit;
  YV12ToRasterization(sour, dest);
  dest.Zoom(oriSiz.width, oriSiz.height);
end;

procedure RasterizationToQuartYUV(Compressed: Boolean; sour: TMemoryRaster; dest: TCoreClassStream);
var
  oriSiz: TOriginSize;
  n: TMemoryRaster;
begin
  oriSiz.width := sour.width;
  oriSiz.height := sour.height;
  dest.write(oriSiz, SizeOf(oriSiz));

  n := TMemoryRaster.Create;
  // smooth level 1
  n.ZoomFrom(sour, sour.width shr 1, sour.height shr 1);
  // smooth level 2
  n.Zoom(n.width shr 1, n.height shr 1);
  RasterizationToYV12(Compressed, n, dest);
  DisposeObject(n);
end;

procedure SaveByteRasterToStream(raster: TByteRaster; stream: TCoreClassStream);
var
  i, w, H: Integer;
begin
  H := length(raster);
  if H > 0 then
      w := length(raster[0])
  else
      w := 0;
  stream.write(w, 4);
  stream.write(H, 4);
  for i := 0 to H - 1 do
      stream.write(raster[i, 0], w);
end;

procedure LoadByteRasterFromStream(var raster: TByteRaster; stream: TCoreClassStream);
var
  i, w, H: Integer;
begin
  stream.read(w, 4);
  stream.read(H, 4);
  SetLength(raster, H, w);
  for i := 0 to H - 1 do
      stream.read(raster[i, 0], w);
end;

procedure SaveWordRasterToStream(raster: TWordRaster; stream: TCoreClassStream);
var
  i, w, H: Integer;
begin
  H := length(raster);
  if H > 0 then
      w := length(raster[0])
  else
      w := 0;
  stream.write(w, 4);
  stream.write(H, 4);
  for i := 0 to H - 1 do
      stream.write(raster[i, 0], w * 2);
end;

procedure LoadWordRasterFromStream(var raster: TWordRaster; stream: TCoreClassStream);
var
  i, w, H: Integer;
begin
  stream.read(w, 4);
  stream.read(H, 4);
  SetLength(raster, H, w);
  for i := 0 to H - 1 do
      stream.read(raster[i, 0], w * 2);
end;
