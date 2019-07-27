function ClampInt(const Value, Min, Max: Integer): Integer;
begin
  if Value > Max then
      Result := Max
  else if Value < Min then
      Result := Min
  else
      Result := Value;
end;

function ClampByte3(const Value, Min, Max: Byte): Byte;
begin
  if Value > Max then
      Result := Max
  else if Value < Min then
      Result := Min
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

procedure OffsetRect_(var r: TRect; dx, dy: Integer);
begin
  inc(r.Left, dx);
  inc(r.Top, dy);
  inc(r.Right, dx);
  inc(r.Bottom, dy);
end;

function IsRectEmpty_(const r: TRect): Boolean;
begin
  Result := (r.Right <= r.Left) or (r.Bottom <= r.Top);
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

procedure FillRasterColor(var x; Count: Cardinal; Value: TRasterColor);
var
  p: PRasterColor;
begin
  p := PRasterColor(@x);
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

function RandomRasterColor(const a: Byte = $FF): TRasterColor;
begin
  Result := RasterColor(Random($FF), Random($FF), Random($FF), a);
end;

function RasterColor(const r, g, b, a: Byte): TRasterColor;
begin
  TRasterColorEntry(Result).r := r;
  TRasterColorEntry(Result).g := g;
  TRasterColorEntry(Result).b := b;
  TRasterColorEntry(Result).a := a;
end;

function RasterColor(const r, g, b: Byte): TRasterColor;
begin
  TRasterColorEntry(Result).r := r;
  TRasterColorEntry(Result).g := g;
  TRasterColorEntry(Result).b := b;
  TRasterColorEntry(Result).a := $FF;
end;

function RasterColorInv(const c: TRasterColor): TRasterColor;
begin
  TRasterColorEntry(Result).r := $FF - TRasterColorEntry(c).r;
  TRasterColorEntry(Result).g := $FF - TRasterColorEntry(c).g;
  TRasterColorEntry(Result).b := $FF - TRasterColorEntry(c).b;
  TRasterColorEntry(Result).a := TRasterColorEntry(c).a;
end;

function RasterAlphaColor(const c: TRasterColor; const a: Byte): TRasterColor;
begin
  if TRasterColorEntry(c).a = 0 then
      Result := 0
  else if TRasterColorEntry(c).a = $FF then
      Result := c
  else
    begin
      TRasterColorEntry(Result).r := Trunc(TRasterColorEntry(c).r * (TRasterColorEntry(c).a / $FF));
      TRasterColorEntry(Result).g := Trunc(TRasterColorEntry(c).g * (TRasterColorEntry(c).a / $FF));
      TRasterColorEntry(Result).b := Trunc(TRasterColorEntry(c).b * (TRasterColorEntry(c).a / $FF));
      TRasterColorEntry(Result).a := TRasterColorEntry(c).a;
    end;
end;

function RasterAlphaColorF(const c: TRasterColor; const a: Single): TRasterColor;
begin
  Result := RasterAlphaColor(c, ClampByte(Trunc(a * $FF)));
end;

function RasterColorF(const r, g, b, a: TGeoFloat): TRasterColor;
begin
  TRasterColorEntry(Result).r := ClampByte(Round(r * $FF));
  TRasterColorEntry(Result).g := ClampByte(Round(g * $FF));
  TRasterColorEntry(Result).b := ClampByte(Round(b * $FF));
  TRasterColorEntry(Result).a := ClampByte(Round(a * $FF));
end;

function RasterColorF(const r, g, b: TGeoFloat): TRasterColor;
begin
  Result := RasterColorF(r, g, b, 1.0);
end;

procedure RasterColor2F(const c: TRasterColor; var r, g, b, a: Single);
begin
  r := TRasterColorEntry(c).r / $FF;
  g := TRasterColorEntry(c).g / $FF;
  b := TRasterColorEntry(c).b / $FF;
  a := TRasterColorEntry(c).a / $FF;
end;

procedure RasterColor2F(const c: TRasterColor; var r, g, b: Single);
begin
  r := TRasterColorEntry(c).r / $FF;
  g := TRasterColorEntry(c).g / $FF;
  b := TRasterColorEntry(c).b / $FF;
end;

function RasterColor2Vec4(const c: TRasterColor): TVec4;
begin
  RasterColor2F(c, Result[0], Result[1], Result[2], Result[3]);
end;

function RasterColor2Vector4(const c: TRasterColor): TVector4;
begin
  RasterColor2F(c, Result.Buff[0], Result.Buff[1], Result.Buff[2], Result.Buff[3]);
end;

function RasterColor2Vec3(const c: TRasterColor): TVec3;
begin
  RasterColor2F(c, Result[0], Result[1], Result[2]);
end;

function RasterColor2Vector3(const c: TRasterColor): TVector3;
begin
  RasterColor2F(c, Result.Buff[0], Result.Buff[1], Result.Buff[2]);
end;

function RasterColor2Gray(const c: TRasterColor): Byte;
begin
  with TRasterColorEntry(c) do
      Result := Round((r + g + b) / 3);
end;

function RasterColor2GrayS(const c: TRasterColor): Single;
begin
  with TRasterColorEntry(c) do
      Result := (r + g + b) / 3 / $FF;
end;

function RasterColor2GrayD(const c: TRasterColor): Double;
begin
  with TRasterColorEntry(c) do
      Result := (r + g + b) / 3 / $FF;
end;

procedure FillRColor(var x; Count: Cardinal; Value: TRColor);
var
  p: PRColor;
begin
  p := PRColor(@x);
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

function RandomRColor(const a: Byte = $FF): TRColor;
begin
  Result := RColor(Random($FF), Random($FF), Random($FF), a);
end;

function RColor(const r, g, b, a: Byte): TRColor;
begin
  TRColorEntry(Result).r := r;
  TRColorEntry(Result).g := g;
  TRColorEntry(Result).b := b;
  TRColorEntry(Result).a := a;
end;

function RColor(const r, g, b: Byte): TRColor;
begin
  TRColorEntry(Result).r := r;
  TRColorEntry(Result).g := g;
  TRColorEntry(Result).b := b;
  TRColorEntry(Result).a := $FF;
end;

function RColorInv(const c: TRColor): TRColor;
begin
  TRColorEntry(Result).r := $FF - TRColorEntry(c).r;
  TRColorEntry(Result).g := $FF - TRColorEntry(c).g;
  TRColorEntry(Result).b := $FF - TRColorEntry(c).b;
  TRColorEntry(Result).a := TRColorEntry(c).a;
end;

function RColorF(const r, g, b, a: TGeoFloat): TRColor;
begin
  TRColorEntry(Result).r := ClampByte(Round(r * $FF));
  TRColorEntry(Result).g := ClampByte(Round(g * $FF));
  TRColorEntry(Result).b := ClampByte(Round(b * $FF));
  TRColorEntry(Result).a := ClampByte(Round(a * $FF));
end;

function RColorF(const r, g, b: TGeoFloat): TRColor;
begin
  Result := RColorF(r, g, b, 1.0);
end;

procedure RColor2F(const c: TRColor; var r, g, b, a: Single);
begin
  r := TRColorEntry(c).r / $FF;
  g := TRColorEntry(c).g / $FF;
  b := TRColorEntry(c).b / $FF;
  a := TRColorEntry(c).a / $FF;
end;

procedure RColor2F(const c: TRColor; var r, g, b: Single);
begin
  r := TRColorEntry(c).r / $FF;
  g := TRColorEntry(c).g / $FF;
  b := TRColorEntry(c).b / $FF;
end;

function RColor2Vec4(const c: TRColor): TVec4;
begin
  RColor2F(c, Result[0], Result[1], Result[2], Result[3]);
end;

function RColor2Vector4(const c: TRColor): TVector4;
begin
  RColor2F(c, Result.Buff[0], Result.Buff[1], Result.Buff[2], Result.Buff[3]);
end;

function RColor2Vec3(const c: TRColor): TVec3;
begin
  RColor2F(c, Result[0], Result[1], Result[2]);
end;

function RColor2Vector3(const c: TRColor): TVector3;
begin
  RColor2F(c, Result.Buff[0], Result.Buff[1], Result.Buff[2]);
end;

function RColor2Gray(const c: TRColor): Byte;
begin
  with TRColorEntry(c) do
      Result := Round((r + g + b) / 3);
end;

function RColor2GrayS(const c: TRColor): Single;
begin
  with TRColorEntry(c) do
      Result := (r + g + b) / 3 / $FF;
end;

function RColor2GrayD(const c: TRColor): Double;
begin
  with TRColorEntry(c) do
      Result := (r + g + b) / 3 / $FF;
end;

function RColorDistance(c1, c2: TRColor): TGeoFloat;
var
  v1, v2: TVector4;
begin
  v1 := RColor2Vector4(c1);
  v2 := RColor2Vector4(c2);
  Result := v1.Distance4D(v2);
end;

function RGBA2BGRA(const sour: TRColor): TRColor;
begin
  TRColorEntry(Result).r := TRColorEntry(sour).b;
  TRColorEntry(Result).g := TRColorEntry(sour).g;
  TRColorEntry(Result).b := TRColorEntry(sour).r;
  TRColorEntry(Result).a := TRColorEntry(sour).a;
end;

function BGRA2RGBA(const sour: TRColor): TRColor;
begin
  TRColorEntry(Result).r := TRColorEntry(sour).b;
  TRColorEntry(Result).g := TRColorEntry(sour).g;
  TRColorEntry(Result).b := TRColorEntry(sour).r;
  TRColorEntry(Result).a := TRColorEntry(sour).a;
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
  TRColorEntry(Result).a := $FF;
end;

procedure SwapBR(var sour: TRGB);
begin
  CoreClasses.Swap(sour[0], sour[2]);
end;

procedure SwapBR(var sour: TRColor);
begin
  CoreClasses.Swap(TRColorEntry(sour).r, TRColorEntry(sour).b);
end;

function MaxRGBComponent(sour: TRColor): Byte;
var
  c: TRColorEntry;
begin
  c.BGRA := sour;
  Result := Max(c.r, Max(c.g, c.b));
end;

function MaxRGBIndex(sour: TRColor): Byte;
var
  c: TRColorEntry;
begin
  c.BGRA := sour;
  if c.Buff[1] > c.Buff[0] then
    begin
      if c.Buff[2] > c.Buff[1] then
          Result := 2
      else
          Result := 1;
    end
  else
    begin
      if c.Buff[2] > c.Buff[0] then
          Result := 2
      else
          Result := 0;
    end;
end;

function MinRGBComponent(sour: TRColor): Byte;
var
  c: TRColorEntry;
begin
  c.BGRA := sour;
  Result := Min(c.r, Min(c.g, c.b));
end;

function MinRGBIndex(sour: TRColor): Byte;
var
  c: TRColorEntry;
begin
  c.BGRA := sour;
  if c.Buff[1] < c.Buff[0] then
    begin
      if c.Buff[2] < c.Buff[1] then
          Result := 2
      else
          Result := 1;
    end
  else
    begin
      if c.Buff[2] < c.Buff[0] then
          Result := 2
      else
          Result := 0;
    end;
end;

function AggColor(const Value: TRColor): TAggColorRgba8;
begin
  Result.r := TRColorEntry(Value).r;
  Result.g := TRColorEntry(Value).g;
  Result.b := TRColorEntry(Value).b;
  Result.a := TRColorEntry(Value).a;
end;

function AggColor(const r, g, b: Single; const a: Single = 1.0): TAggColorRgba8;
begin
  Result := AggColor(RColorF(r, g, b, a));
end;

function AggColor(const Value: TAggColorRgba8): TRColor;
begin
  TRColorEntry(Result).r := Value.r;
  TRColorEntry(Result).g := Value.g;
  TRColorEntry(Result).b := Value.b;
  TRColorEntry(Result).a := Value.a;
end;

function ComputeSize(const MAX_Width, MAX_Height: Integer; var width, height: Integer): Single;
var
  fw, fh: Single;
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
    b, g, r, a, Sum: Integer;
  end;
var
  LL, RR, TT, BB, xx, yy, i, j, x, y, RadiusI, Passes: Integer;
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
  for y := RecTop to RecBottom do
    begin
      ImgPixel := PRColorEntry(dest.ScanLine[y]);
      inc(ImgPixel, RecLeft);
      for x := RecLeft to RecRight do
        with ImgPixel^ do
          begin
            r := DivTable[r, a];
            g := DivTable[g, a];
            b := DivTable[b, a];
            inc(ImgPixel);
          end;
    end;

  for i := 1 to Passes do
    begin
      // horizontal pass...
      for y := RecTop to RecBottom do
        begin
          ImagePixel := PRColorEntry(@dest.ScanLine[y]^[RecLeft]);
          // fill the Pixels buffer with a copy of the row's pixels ...
          CopyRColor(ImagePixel^, pixels[RecLeft], RecRight - RecLeft + 1);

          SumRec.a := 0;
          SumRec.r := 0;
          SumRec.g := 0;
          SumRec.b := 0;
          SumRec.Sum := 0;

          LL := RecLeft;
          RR := RecLeft + RadiusI;
          if RR > RecRight then
              RR := RecRight;
          // update first in row ...
          for xx := LL to RR do
            with pixels[xx] do
              begin
                inc(SumRec.a, a);
                inc(SumRec.r, r);
                inc(SumRec.g, g);
                inc(SumRec.b, b);
                inc(SumRec.Sum);
              end;
          with ImagePixel^ do
            begin
              a := SumRec.a div SumRec.Sum;
              r := SumRec.r div SumRec.Sum;
              g := SumRec.g div SumRec.Sum;
              b := SumRec.b div SumRec.Sum;
            end;
          // update the remaining pixels in the row ...
          for x := RecLeft + 1 to RecRight do
            begin
              inc(ImagePixel);
              LL := x - RadiusI - 1;
              RR := x + RadiusI;
              if LL >= RecLeft then
                with pixels[LL] do
                  begin
                    dec(SumRec.a, a);
                    dec(SumRec.r, r);
                    dec(SumRec.g, g);
                    dec(SumRec.b, b);
                    dec(SumRec.Sum);
                  end;
              if RR <= RecRight then
                with pixels[RR] do
                  begin
                    inc(SumRec.a, a);
                    inc(SumRec.r, r);
                    inc(SumRec.g, g);
                    inc(SumRec.b, b);
                    inc(SumRec.Sum);
                  end;
              with ImagePixel^ do
                begin
                  a := SumRec.a div SumRec.Sum;
                  r := SumRec.r div SumRec.Sum;
                  g := SumRec.g div SumRec.Sum;
                  b := SumRec.b div SumRec.Sum;
                end;
            end;
        end;

      // vertical pass...
      for x := RecLeft to RecRight do
        begin
          ImagePixel := PRColorEntry(@dest.ScanLine[RecTop]^[x]);
          for j := RecTop to RecBottom do
            begin
              pixels[j] := ImagePixel^;
              inc(ImagePixel, dest.width);
            end;
          ImagePixel := PRColorEntry(@dest.ScanLine[RecTop]^[x]);

          TT := RecTop;
          BB := RecTop + RadiusI;
          if BB > RecBottom then
              BB := RecBottom;
          SumRec.a := 0;
          SumRec.r := 0;
          SumRec.g := 0;
          SumRec.b := 0;
          SumRec.Sum := 0;
          // update first in col ...
          for yy := TT to BB do
            with pixels[yy] do
              begin
                inc(SumRec.a, a);
                inc(SumRec.r, r);
                inc(SumRec.g, g);
                inc(SumRec.b, b);
                inc(SumRec.Sum);
              end;
          with ImagePixel^ do
            begin
              a := SumRec.a div SumRec.Sum;
              r := SumRec.r div SumRec.Sum;
              g := SumRec.g div SumRec.Sum;
              b := SumRec.b div SumRec.Sum;
            end;
          // update remainder in col ...
          for y := RecTop + 1 to RecBottom do
            begin
              inc(ImagePixel, dest.width);
              TT := y - RadiusI - 1;
              BB := y + RadiusI;

              if TT >= RecTop then
                with pixels[TT] do
                  begin
                    dec(SumRec.a, a);
                    dec(SumRec.r, r);
                    dec(SumRec.g, g);
                    dec(SumRec.b, b);
                    dec(SumRec.Sum);
                  end;
              if BB <= RecBottom then
                with pixels[BB] do
                  begin
                    inc(SumRec.a, a);
                    inc(SumRec.r, r);
                    inc(SumRec.g, g);
                    inc(SumRec.b, b);
                    inc(SumRec.Sum);
                  end;
              with ImagePixel^ do
                begin
                  a := SumRec.a div SumRec.Sum;
                  r := SumRec.r div SumRec.Sum;
                  g := SumRec.g div SumRec.Sum;
                  b := SumRec.b div SumRec.Sum;
                end;
            end;
        end;
    end;

  // extract alphas ...
  for y := RecTop to RecBottom do
    begin
      ImgPixel := PRColorEntry(@dest.ScanLine[y]^[RecLeft]);
      for x := RecLeft to RecRight do
        begin
          ImgPixel^.r := RcTable[ImgPixel^.a, ImgPixel^.r];
          ImgPixel^.g := RcTable[ImgPixel^.a, ImgPixel^.g];
          ImgPixel^.b := RcTable[ImgPixel^.a, ImgPixel^.b];
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
    b, g, r, a: Int64;
    Sum: Integer;
  end;
var
  q, i, j, x, y, ImageWidth, RowOffset, RadiusI: Integer;
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
  for y := RecTop to RecBottom do
    begin
      // initialize PreMulArray for the row ...
      q := (y * ImageWidth) + RecLeft;
      for x := RecLeft to RecRight do
        with ImagePixels^[q] do
          begin
            PreMulArray[x].a := a;
            PreMulArray[x].r := DivTable[r, a];
            PreMulArray[x].g := DivTable[g, a];
            PreMulArray[x].b := DivTable[b, a];
            inc(q);
          end;

      for x := RecLeft to RecRight do
        begin
          SumRec.a := 0;
          SumRec.r := 0;
          SumRec.g := 0;
          SumRec.b := 0;
          SumRec.Sum := 0;

          i := Max(x - RadiusI, RecLeft);
          q := i - (x - RadiusI);
          for i := i to Min(x + RadiusI, RecRight) do
            with PreMulArray[i] do
              begin
                inc(SumRec.a, GaussLUT[q][a]);
                inc(SumRec.r, GaussLUT[q][r]);
                inc(SumRec.g, GaussLUT[q][g]);
                inc(SumRec.b, GaussLUT[q][b]);
                inc(SumRec.Sum, GaussLUT[q][1]);
                inc(q);
              end;
          q := RowOffset + x;
          SumArray[q].a := SumRec.a div SumRec.Sum;
          SumArray[q].r := SumRec.r div SumRec.Sum;
          SumArray[q].g := SumRec.g div SumRec.Sum;
          SumArray[q].b := SumRec.b div SumRec.Sum;
        end;
      inc(RowOffset, ImageWidth);
    end;

  if Source <> dest then
      dest.SetSize(Source.width, Source.height);

  RowOffset := RecTop * ImageWidth;
  for y := RecTop to RecBottom do
    begin
      for x := RecLeft to RecRight do
        begin
          SumRec.a := 0;
          SumRec.r := 0;
          SumRec.g := 0;
          SumRec.b := 0;
          SumRec.Sum := 0;

          i := Max(y - RadiusI, RecTop);
          q := i - (y - RadiusI);
          for i := i to Min(y + RadiusI, RecBottom) do
            with SumArray[x + i * ImageWidth] do
              begin
                inc(SumRec.a, GaussLUT[q][a]);
                inc(SumRec.r, GaussLUT[q][r]);
                inc(SumRec.g, GaussLUT[q][g]);
                inc(SumRec.b, GaussLUT[q][b]);
                inc(SumRec.Sum, GaussLUT[q][1]);
                inc(q);
              end;

          SourPixel := @ImagePixels^[RowOffset + x];

          if Source <> dest then
              DestPixel := @PRColorEntryArray(dest.FBits)^[RowOffset + x]
          else
              DestPixel := SourPixel;

          DestPixel^.a := (SumRec.a div SumRec.Sum);
          DestPixel^.r := RcTable[DestPixel^.a, (SumRec.r div SumRec.Sum)];
          DestPixel^.g := RcTable[DestPixel^.a, (SumRec.g div SumRec.Sum)];
          DestPixel^.b := RcTable[DestPixel^.a, (SumRec.b div SumRec.Sum)];
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
    r, a: Int64;
    Sum: Integer;
  end;
var
  q, i, j, x, y, ImageWidth, RowOffset, RadiusI: Integer;
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
  for y := RecTop to RecBottom do
    begin
      // initialize PreMulArray for the row ...
      q := (y * ImageWidth) + RecLeft;
      for x := RecLeft to RecRight do
        with ImagePixels^[q] do
          begin
            PreMulArray[x].a := a;
            PreMulArray[x].r := DivTable[RColor2Gray(BGRA), a];
            inc(q);
          end;

      for x := RecLeft to RecRight do
        begin
          SumRec.a := 0;
          SumRec.r := 0;
          SumRec.Sum := 0;

          i := Max(x - RadiusI, RecLeft);
          q := i - (x - RadiusI);
          for i := i to Min(x + RadiusI, RecRight) do
            with PreMulArray[i] do
              begin
                inc(SumRec.a, GaussLUT[q][a]);
                inc(SumRec.r, GaussLUT[q][r]);
                inc(SumRec.Sum, GaussLUT[q][1]);
                inc(q);
              end;
          q := RowOffset + x;
          SumArray[q].a := SumRec.a div SumRec.Sum;
          SumArray[q].r := SumRec.r div SumRec.Sum;
        end;
      inc(RowOffset, ImageWidth);
    end;

  if Source <> dest then
      dest.SetSize(Source.width, Source.height);

  RowOffset := RecTop * ImageWidth;
  for y := RecTop to RecBottom do
    begin
      for x := RecLeft to RecRight do
        begin
          SumRec.a := 0;
          SumRec.r := 0;
          SumRec.Sum := 0;

          i := Max(y - RadiusI, RecTop);
          q := i - (y - RadiusI);
          for i := i to Min(y + RadiusI, RecBottom) do
            with SumArray[x + i * ImageWidth] do
              begin
                inc(SumRec.a, GaussLUT[q][a]);
                inc(SumRec.r, GaussLUT[q][r]);
                inc(SumRec.Sum, GaussLUT[q][1]);
                inc(q);
              end;

          SourPixel := @ImagePixels^[RowOffset + x];

          if Source <> dest then
              DestPixel := @PRColorEntryArray(dest.FBits)^[RowOffset + x]
          else
              DestPixel := SourPixel;

          DestPixel^.a := (SumRec.a div SumRec.Sum);
          DestPixel^.r := RcTable[DestPixel^.a, (SumRec.r div SumRec.Sum)];
          DestPixel^.g := DestPixel^.r;
          DestPixel^.b := DestPixel^.g;
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
  LMemo, x, y: Integer;
  a, r, g, b: Cardinal;
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

  for y := AYOrigin to AYFinal do
    begin
      P0 := DestMR.ScanLine[y - 1];
      p1 := DestMR.ScanLine[y];
      p2 := DestMR.ScanLine[y + 1];

      for x := AXOrigin to AXFinal do
        begin
          // alpha component
          A0 := P0^[x] shr 24 and $FF;
          A1Prev := p1^[x - 1] shr 24 and $FF;
          A1Next := p1^[x + 1] shr 24 and $FF;
          a2 := p2^[x] shr 24 and $FF;

          // red component
          r0 := P0^[x] shr 16 and $FF;
          R1Prev := p1^[x - 1] shr 16 and $FF;
          R1Next := p1^[x + 1] shr 16 and $FF;
          r2 := p2^[x] shr 16 and $FF;

          // green component
          G0 := P0^[x] shr 8 and $FF;
          G1Prev := p1^[x - 1] shr 8 and $FF;
          G1Next := p1^[x + 1] shr 8 and $FF;
          g2 := p2^[x] shr 8 and $FF;

          // blue component
          b0 := P0^[x] and $FF;
          B1Prev := p1^[x - 1] and $FF;
          B1Next := p1^[x + 1] and $FF;
          b2 := p2^[x] and $FF;

          // composition
          a := (A0 + a2 + A1Prev + A1Next) div 4;
          r := (r0 + r2 + R1Prev + R1Next) div 4;
          g := (G0 + g2 + G1Prev + G1Next) div 4;
          b := (b0 + b2 + B1Prev + B1Next) div 4;

          p1^[x] := (a shl 24) or (r shl 16) or (g shl 8) or b;
        end;
    end;
end;

procedure Antialias32(const DestMR: TMemoryRaster; const AAmount: Integer);
var
  i: Integer;
begin
  if AAmount >= 1 then
    for i := 1 to AAmount do
        Antialias32(DestMR, 0, 0, DestMR.width, DestMR.height);
end;

procedure HistogramEqualize(const mr: TMemoryRaster);
var
  LHistogram: array [0 .. $FF] of Cardinal;
  LMap: array [0 .. $FF] of Byte;
  i: Integer;
  LPixelCount: Integer;
  r, g, b: Byte;
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
      r := p^ shr 16 and $FF;
      g := p^ shr 8 and $FF;
      b := p^ and $FF;

      LHistogram[r] := LHistogram[r] + 1;
      LHistogram[g] := LHistogram[g] + 1;
      LHistogram[b] := LHistogram[b] + 1;

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
      r := p^ shr 16 and $FF;
      g := p^ shr 8 and $FF;
      b := p^ and $FF;

      r := LMap[r];
      g := LMap[g];
      b := LMap[b];

      p^ := (p^ and $FF000000) or (r shl 16) or (g shl 8) or b;

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
  r, g, b: Byte;
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
      r := p^ shr 16 and $FF;
      g := p^ shr 8 and $FF;
      b := p^ and $FF;
      LHistogram1[r] := LHistogram1[r] + 1;
      LHistogram1[g] := LHistogram1[g] + 1;
      LHistogram1[b] := LHistogram1[b] + 1;
      inc(p);
    end;

  mr2.ReadyBits();
  p := @mr2.FBits^[0];
  LPixelCount := mr2.width * mr2.height;

  for i := 1 to LPixelCount do
    begin
      r := p^ shr 16 and $FF;
      g := p^ shr 8 and $FF;
      b := p^ and $FF;
      LHistogram2[r] := LHistogram2[r] + 1;
      LHistogram2[g] := LHistogram2[g] + 1;
      LHistogram2[b] := LHistogram2[b] + 1;
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
      r := p^ shr 16 and $FF;
      g := p^ shr 8 and $FF;
      b := p^ and $FF;
      r := umlMin(LMap1[r], LMap2[r]);
      g := umlMin(LMap1[g], LMap2[g]);
      b := umlMin(LMap1[b], LMap2[b]);
      p^ := (p^ and $FF000000) or (r shl 16) or (g shl 8) or b;
      inc(p);
    end;

  mr2.ReadyBits();
  p := @mr2.FBits^[0];
  LPixelCount := mr2.width * mr2.height;
  for i := 1 to LPixelCount do
    begin
      r := p^ shr 16 and $FF;
      g := p^ shr 8 and $FF;
      b := p^ and $FF;
      r := umlMin(LMap1[r], LMap2[r]);
      g := umlMin(LMap1[g], LMap2[g]);
      b := umlMin(LMap1[b], LMap2[b]);
      p^ := (p^ and $FF000000) or (r shl 16) or (g shl 8) or b;
      inc(p);
    end;
end;

procedure RemoveRedEyes(const mr: TMemoryRaster);
var
  x, y: Integer;
  w, h: Integer;
  pixptr: PRColorEntry;
  nrv, bluf, redq: Single;
  powr, powb, powg: Single;
begin
  w := mr.width;
  h := mr.height;

  for y := 0 to (h - 1) do
    begin
      for x := 0 to (w - 1) do
        begin
          pixptr := PRColorEntry(mr.PixelPtr[x, y]);
          nrv := pixptr^.g + pixptr^.b;

          if nrv < 1 then
              nrv := 1;

          if pixptr^.g > 1 then
              bluf := pixptr^.b / pixptr^.g
          else
              bluf := pixptr^.b;

          bluf := Max(0.5, Min(1.5, Sqrt(bluf)));
          redq := (pixptr^.r / nrv) * bluf;

          if redq > 0.7 then
            begin
              powr := 1.775 - (redq * 0.75 + 0.25);

              if powr < 0 then
                  powr := 0;

              powr := powr * powr;
              powb := 1 - (1 - powr) / 2;
              powg := 1 - (1 - powr) / 4;

              pixptr^.r := Round(powr * pixptr^.r);
              pixptr^.b := Round(powb * pixptr^.b);
              pixptr^.g := Round(powg * pixptr^.g);
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
      LPixel^.b := (LPixel^.r + LPixel^.g + LPixel^.b) div 3;

      // set red component of sepia color
      LPixel^.r := Byte(LPixel^.b + LDepth2);

      if LPixel^.r < LDepth2 then
          LPixel^.r := $FF;

      // set green component of sepia color
      LPixel^.g := Byte(LPixel^.b + Depth);

      if LPixel^.g < Depth then
          LPixel^.g := $FF;

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
  i, x, y, ix, iy, dx: Integer;
  LDiagonal: Integer;
  LDiagonalX: Integer;
  LDiagonalY: Integer;
  AA, RR, GG, BB: Integer;
  a, r, g, b: Byte;
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

    for y := 0 to (DestMR.height - 1) do
      begin
        for x := 0 to (DestMR.width - 1) do
          begin
            AA := 0;
            RR := 0;
            GG := 0;
            BB := 0;

            for LDiagonalY := -LDiagonal to LDiagonal do
              begin
                for LDiagonalX := -LDiagonal to LDiagonal do
                  begin
                    iy := y + LDiagonalY;
                    ix := x + LDiagonalX;

                    { The original routines in the following checking code was
                      if  (iy >= 1) ...
                      and (ix >= 1) ... }
                    if (iy >= 0) and
                      (ix >= 0) and
                      (iy <= (DestMR.height - 1)) and
                      (ix <= (DestMR.width - 1)) then
                      begin
                        a := LOriginalRow[iy]^[ix] shr 24 and $FF;
                        r := LOriginalRow[iy]^[ix] shr 16 and $FF;
                        g := LOriginalRow[iy]^[ix] shr 8 and $FF;
                        b := LOriginalRow[iy]^[ix] and $FF;
                      end
                    else
                      begin
                        a := LOriginalRow[y]^[x] shr 24 and $FF;
                        r := LOriginalRow[y]^[x] shr 16 and $FF;
                        g := LOriginalRow[y]^[x] shr 8 and $FF;
                        b := LOriginalRow[y]^[x] and $FF;
                      end;

                    i := 12 + LDiagonalY * 5 + LDiagonalX;
                    AA := AA + a * LMatrix[i];
                    RR := RR + r * LMatrix[i];
                    GG := GG + g * LMatrix[i];
                    BB := BB + b * LMatrix[i];
                  end;
              end;

            AA := AA div LLastMatrixNumber;
            RR := RR div LLastMatrixNumber;
            GG := GG div LLastMatrixNumber;
            BB := BB div LLastMatrixNumber;

            a := ClampInt(AA, 0, $FF);
            r := ClampInt(RR, 0, $FF);
            g := ClampInt(GG, 0, $FF);
            b := ClampInt(BB, 0, $FF);

            LDestRow[y]^[x] := (a shl 24) or (r shl 16) or (g shl 8) or b;
          end;
      end;
  finally
    DisposeObject(LMRCopy);
    SetLength(LDestRow, 0);
    SetLength(LOriginalRow, 0);
  end;
end;

procedure AddColorNoise32(const Source: TMemoryRaster; const AAmount: Integer);
var
  i, RR, GG, BB, LAmount: Integer;
  a, r, g, b: Cardinal;
  p: PRColor;
begin
  LAmount := AAmount;

  if LAmount < 0 then
    begin
      LAmount := 0;
    end;

  p := @Source.Bits^[0];

  for i := 0 to (Source.width * Source.height - 1) do
    begin
      a := p^ and $FF000000;

      if a > 0 then
        begin
          RR := p^ shr 16 and $FF;
          GG := p^ shr 8 and $FF;
          BB := p^ and $FF;

          RR := RR + (Random(LAmount) - (LAmount shr 1));
          GG := GG + (Random(LAmount) - (LAmount shr 1));
          BB := BB + (Random(LAmount) - (LAmount shr 1));

          r := IfThen(RR > $FF, $FF, RR);
          g := IfThen(GG > $FF, $FF, RR);
          b := IfThen(BB > $FF, $FF, RR);
          p^ := a or (r shl 16) or (g shl 8) or b;
        end;

      inc(p);
    end;
end;

procedure AddMonoNoise32(const Source: TMemoryRaster; const AAmount: Integer);
var
  i, LDelta, LAmount: Integer;
  RR, GG, BB: Integer;
  a, r, g, b: Cardinal;
  p: PRColor;
begin
  LAmount := AAmount;

  if LAmount < 0 then
    begin
      LAmount := 0;
    end;

  p := @Source.Bits^[0];

  for i := 0 to (Source.width * Source.height - 1) do
    begin
      a := p^ and $FF000000;

      if a > 0 then
        begin
          LDelta := Random(LAmount) - (LAmount shr 1);

          RR := p^ shr 16 and $FF;
          GG := p^ shr 8 and $FF;
          BB := p^ and $FF;

          RR := RR + LDelta;
          GG := GG + LDelta;
          BB := BB + LDelta;

          r := IfThen(RR > $FF, $FF, RR);
          g := IfThen(GG > $FF, $FF, RR);
          b := IfThen(BB > $FF, $FF, RR);
          p^ := a or (r shl 16) or (g shl 8) or b;
        end;

      inc(p);
    end;
end;

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
  c: PRColorEntry;
begin
  Src.ReadyBits();
  for i := (Src.width * Src.height) - 1 downto 0 do
    begin
      c := @Src.FBits^[i];
      c^.a := RColor2Gray(c^.BGRA);
    end;
end;

procedure AlphaToGrayscale(Src: TMemoryRaster);
var
  i: Integer;
  c: PRColorEntry;
begin
  Src.ReadyBits();
  for i := (Src.width * Src.height) - 1 downto 0 do
    begin
      c := @Src.FBits^[i];
      c^.r := c^.a;
      c^.g := c^.a;
      c^.b := c^.a;
    end;
end;

procedure IntensityToAlpha(Src: TMemoryRaster);
var
  i: Integer;
  c: PRColorEntry;
  f: Single;
begin
  Src.ReadyBits();
  for i := (Src.width * Src.height) - 1 downto 0 do
    begin
      c := @Src.FBits^[i];
      c^.a := ((c^.r * 61 + c^.g * 174 + c^.b * 21) shr 8);
    end;
end;

procedure ReversalAlpha(Src: TMemoryRaster);
var
  i: Integer;
  c: PRColorEntry;
begin
  Src.ReadyBits();
  for i := (Src.width * Src.height) - 1 downto 0 do
    begin
      c := @Src.FBits^[i];
      c^.a := $FF - c^.a;
    end;
end;

procedure RGBToGrayscale(Src: TMemoryRaster);
var
  i: Integer;
  c: PRColorEntry;
begin
  Src.ReadyBits();
  for i := (Src.width * Src.height) - 1 downto 0 do
    begin
      c := @Src.FBits^[i];
      c^.r := RColor2Gray(c^.BGRA);
      c^.g := c^.r;
      c^.b := c^.r;
    end;
end;

procedure FillBlackGrayBackgroundTexture(bk: TMemoryRaster; block_size: Integer);
var
  i, j: Integer;
  fi, fj: Boolean;
  c, c1, c2: TRColor;
begin
  fi := True;
  i := 0;
  c1 := RColorF(0.1, 0.1, 0.1, 1);
  c2 := RColorF(0.2, 0.2, 0.2, 1);
  while i < bk.width do
    begin
      fi := not fi;
      fj := fi;
      j := 0;
      while j < bk.height do
        begin
          if fj then
              c := c1
          else
              c := c2;
          fj := not fj;
          bk.FillRect(i, j, i + block_size, j + block_size, c);
          inc(j, block_size);
        end;
      inc(i, block_size);
    end;
end;

procedure ColorToTransparent(SrcColor: TRColor; Src, Dst: TMemoryRaster);
var
  i, j: Integer;
  c: TRColorEntry;
begin
  CheckParams(Src, Dst);
  for i := 0 to Src.width - 1 do
    for j := 0 to Src.height - 1 do
      begin
        c.BGRA := Src[i, j];
        if c.BGRA = SrcColor then
            Dst[i, j] := RColor(0, 0, 0, 0)
        else
            Dst[i, j] := c.BGRA;
      end;
end;

function BuildSequenceFrame(bmp32List: TCoreClassListForObj; Column: Integer; Transparent: Boolean): TSequenceMemoryRaster;
var
  c: TRColor;
  bmp: TMemoryRaster;
  AMaxWidth, AMaxHeight: Integer;
  i: Integer;
  idx, x, y: Integer;
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
      c := RColor(0, 0, 0, 0)
  else
      c := RColor(0, 0, 0, 1);

  Result.SetSize(AMaxWidth * Column, AMaxHeight * rowcnt, c);

  idx := 0;
  x := 0;
  y := 0;

  for i := 0 to bmp32List.Count - 1 do
    begin
      bmp := bmp32List[i] as TMemoryRaster;
      if (bmp.width <> AMaxWidth) or (bmp.height <> AMaxHeight) then
        begin
          newbmp := TMemoryRaster.Create;
          newbmp.ZoomFrom(bmp, AMaxWidth, AMaxHeight);
          BlockTransfer(Result, x, y, Result.BoundsRect, newbmp, newbmp.BoundsRect, dmOpaque);
          DisposeObject(newbmp);
        end
      else
        begin
          BlockTransfer(Result, x, y, Result.BoundsRect, bmp, bmp.BoundsRect, dmOpaque);
        end;

      if idx + 1 >= Column then
        begin
          idx := 0;
          x := 0;
          y := y + AMaxHeight;
        end
      else
        begin
          inc(idx);
          x := x + AMaxWidth;
        end;
    end;

  Result.Total := bmp32List.Count;
  Result.Column := Column;
end;

function GetSequenceFrameRect(bmp: TMemoryRaster; Total, Column, index: Integer): TRect;
var
  rowIdx, colIdx: Integer;
  Row: Integer;
  AWidth, AHeight: Integer;
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

  AWidth := bmp.width div Column;
  AHeight := bmp.height div Row;

  Result := Rect(colIdx * AWidth, rowIdx * AHeight, (colIdx + 1) * AWidth, (rowIdx + 1) * AHeight);
end;

procedure GetSequenceFrameOutput(bmp: TMemoryRaster; Total, Column, index: Integer; output: TMemoryRaster);
var
  r: TRect;
  w, h: Integer;
begin
  r := GetSequenceFrameRect(bmp, Total, Column, index);
  w := r.Right - r.Left;
  h := r.Bottom - r.Top;
  output.SetSize(w, h);
  BlockTransfer(output, 0, 0, output.BoundsRect, bmp, r, dmOpaque);
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

function BlendReg(f, b: TRColor): TRColor;
var
  FX: TRColorEntry absolute f;
  BX: TRColorEntry absolute b;
  AF, AB: PByteArray;
  FA: Byte;
begin
  FA := FX.a;

  if FA = 0 then
    begin
      Result := b;
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
      r := AF^[FX.r] + AB^[r];
      g := AF^[FX.g] + AB^[g];
      b := AF^[FX.b] + AB^[b];
    end;
  Result := b;
end;

procedure BlendMem(f: TRColor; var b: TRColor);
var
  FX: TRColorEntry absolute f;
  BX: TRColorEntry absolute b;
  AF, AB: PByteArray;
  FA: Byte;
begin
  FA := FX.a;

  if FA = 0 then
      Exit;

  if FA = $FF then
    begin
      b := f;
      Exit;
    end;

  with BX do
    begin
      AF := @DivTable[FA];
      AB := @DivTable[not FA];
      r := AF^[FX.r] + AB^[r];
      g := AF^[FX.g] + AB^[g];
      b := AF^[FX.b] + AB^[b];
    end;
end;

function BlendRegEx(f, b, M: TRColor): TRColor;
var
  FX: TRColorEntry absolute f;
  BX: TRColorEntry absolute b;
  AF, AB: PByteArray;
begin
  AF := @DivTable[M];

  M := AF^[FX.a];

  if M = 0 then
    begin
      Result := b;
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
      r := AF^[FX.r] + AB^[r];
      g := AF^[FX.g] + AB^[g];
      b := AF^[FX.b] + AB^[b];
    end;
  Result := b;
end;

procedure BlendMemEx(f: TRColor; var b: TRColor; M: TRColor);
var
  FX: TRColorEntry absolute f;
  BX: TRColorEntry absolute b;
  AF, AB: PByteArray;
begin
  AF := @DivTable[M];

  M := AF^[FX.a];

  if M = 0 then
    begin
      Exit;
    end;

  if M >= $FF then
    begin
      b := f;
      Exit;
    end;

  with BX do
    begin
      AF := @DivTable[M];
      AB := @DivTable[$FF - M];
      r := AF^[FX.r] + AB^[r];
      g := AF^[FX.g] + AB^[g];
      b := AF^[FX.b] + AB^[b];
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

function CombineReg(x, y, w: TRColor): TRColor;
var
  XE: TRColorEntry absolute x;
  YE: TRColorEntry absolute y;
  AF, AB: PByteArray;
begin
  if w = 0 then
    begin
      Result := y;
      Exit;
    end;

  if w >= $FF then
    begin
      Result := x;
      Exit;
    end;

  with XE do
    begin
      AF := @DivTable[w];
      AB := @DivTable[$FF - w];
      r := AB^[YE.r] + AF^[r];
      g := AB^[YE.g] + AF^[g];
      b := AB^[YE.b] + AF^[b];
    end;
  Result := x;
end;

procedure CombineMem(x: TRColor; var y: TRColor; w: TRColor);
var
  XE: TRColorEntry absolute x;
  YE: TRColorEntry absolute y;
  AF, AB: PByteArray;
begin
  if w = 0 then
    begin
      Exit;
    end;

  if w >= $FF then
    begin
      y := x;
      Exit;
    end;

  with XE do
    begin
      AF := @DivTable[w];
      AB := @DivTable[$FF - w];
      r := AB^[YE.r] + AF^[r];
      g := AB^[YE.g] + AF^[g];
      b := AB^[YE.b] + AF^[b];
    end;
  y := x;
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

function MergeReg(f, b: TRColor): TRColor;
var
  FA, BA, WA: TRColor;
  fw, BW: PByteArray;
  FX: TRColorEntry absolute f;
  BX: TRColorEntry absolute b;
  RX: TRColorEntry absolute Result;
begin
  FA := f shr 24;
  BA := b shr 24;
  if FA = $FF then
      Result := f
  else if FA = $0 then
      Result := b
  else if BA = $0 then
      Result := f
  else
    begin
      RX.a := DivTable[FA xor $FF, BA xor $FF] xor $FF;
      WA := RcTable[RX.a, FA];
      fw := @DivTable[WA];
      BW := @DivTable[WA xor $FF];
      RX.r := fw^[FX.r] + BW^[BX.r];
      RX.g := fw^[FX.g] + BW^[BX.g];
      RX.b := fw^[FX.b] + BW^[BX.b];
    end;
end;

function MergeRegEx(f, b, M: TRColor): TRColor;
begin
  Result := MergeReg(DivTable[M, f shr 24] shl 24 or f and $00FFFFFF, b);
end;

procedure MergeMem(f: TRColor; var b: TRColor);
begin
  b := MergeReg(f, b);
end;

procedure MergeMemEx(f: TRColor; var b: TRColor; M: TRColor);
begin
  b := MergeReg(DivTable[M, f shr 24] shl 24 or f and $00FFFFFF, b);
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
          Buf[n] := pce.r;
          Buf[n + 1] := pce.g;
          Buf[n + 2] := pce.b;
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
  r, g, b, a: Byte;
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
                  r := Word(((srcword^ and $F800) shr 8)); // to rgb888
                  g := Word(((srcword^ and $07E0) shr 3));
                  b := Word(((srcword^ and $001F) shl 3));
                  output.PixelGray[i, j] := ((r shl 1) + (g shl 2) + g + b) shr 3;
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
                r := Src^;
                inc(Src);
                g := Src^;
                inc(Src);
                b := Src^;
                inc(Src);
                output.Pixel[i, j] := RColor(r, g, b, $FF);
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

{
  compute rotation angle for 8bit Style gray image.
  Useful for finding skew of scanned documents etc.
  Uses Hough transform internally.
  MaxAngle is maximal (abs. value) expected skew angle in degrees (to speed things up) and
  Threshold (0..$FF) is used to classify pixel as black (text) or white (background).
  Area of interest rectangle can be defined to restrict the detection to work only in defined part of image
  (useful when the document has text only in smaller area of page and non-text features outside the area confuse the rotation detector).
  Various calculations stats can be retrieved by passing Stats parameter.
}
function DocmentRotationDetected(const MaxAngle: TGeoFloat; const Treshold: Integer; raster: TMemoryRaster): TGeoFloat;
const
  // Number of "best" lines we take into account when determining
  // resulting rotation angle (lines with most votes).
  BestLinesCount = 40;
  // Angle step used in alpha parameter quantization
  AlphaStep = 0.1;

type
  TLine = record
    Count: Integer;
    index: Integer;
    alpha: TGeoFloat;
    Distance: TGeoFloat;
  end;

  TLineArray = array of TLine;
var
  AlphaStart, MinDist, SumAngles: TGeoFloat;
  AlphaSteps, DistCount, AccumulatorSize, i, AccumulatedCounts: Integer;
  BestLines: TLineArray;
  HoughAccumulator: array of Integer;
  PageWidth, PageHeight: Integer;
  ContentRect: TRect;

  // compute final angle for given angle step.
  function GetFinalAngle(StepIndex: Integer): TGeoFloat;
  begin
    Result := AlphaStart + StepIndex * AlphaStep;
  end;

// compute angle and distance parameters for all lines
// going through point [X, Y].
  procedure computeLines(x, y: Integer);
  var
    D, Rads: TGeoFloat;
    i, DIndex, index: Integer;
    Sin, Cos: TGeoFloat;
  begin
    for i := 0 to AlphaSteps - 1 do
      begin
        // Angle for current step in radians
        Rads := GetFinalAngle(i) * PI / 180;
        SinCos(Rads, Sin, Cos);
        // Parameter D(distance from origin) of the line y=tg(alpha)x + d
        D := y * Cos - x * Sin;
        // compute index into accumulator for current line
        DIndex := Trunc(D - MinDist);
        Index := DIndex * AlphaSteps + i;
        // Add one vote for current line
        HoughAccumulator[Index] := HoughAccumulator[Index] + 1;
      end;
  end;

// Uses Hough transform to calculate all lines that intersect
// interesting points (those classified as beign on base line of the text).
  procedure computeHoughTransform;
  var
    y, x: Integer;
  begin
    for y := 0 to PageHeight - 1 do
      for x := 0 to PageWidth - 1 do
        if (raster.PixelGray[ContentRect.Left + x, ContentRect.Top + y] < Treshold) and
          (raster.PixelGray[ContentRect.Left + x, ContentRect.Top + y + 1] >= Treshold) then
            computeLines(x, y);
  end;

// Chooses "best" lines (with the most votes) from the accumulator
  function GetBestLines(Count: Integer): TLineArray;
  var
    i, j, DistIndex, AlphaIndex: Integer;
    Temp: TLine;
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

begin
  // Use supplied page content rect or just the whole image
  ContentRect := Rect(0, 0, raster.width, raster.height);

  PageWidth := ContentRect.Right - ContentRect.Left;
  PageHeight := ContentRect.Bottom - ContentRect.Top;
  if (ContentRect.Bottom = raster.height) then
      dec(PageHeight); // Don't check for black pixels outsize of image in computeHoughTransform()

  AlphaStart := -MaxAngle;
  AlphaSteps := Ceil(2 * MaxAngle / AlphaStep); // Number of angle steps = samples from interval <-MaxAngle, MaxAngle>
  MinDist := -Max(PageWidth, PageHeight);
  DistCount := 2 * (PageWidth + PageHeight);

  // Determine the size of line accumulator
  AccumulatorSize := DistCount * AlphaSteps;
  SetLength(HoughAccumulator, AccumulatorSize);

  // computeulate Hough transform
  computeHoughTransform;

  // Get the best lines with most votes
  BestLines := GetBestLines(BestLinesCount);

  // Average angles of the selected lines to get the rotation angle of the image
  SumAngles := 0;
  for i := 0 to BestLinesCount - 1 do
      SumAngles := SumAngles + BestLines[i].alpha;

  Result := NormalizeDegAngle(SumAngles / BestLinesCount);
  (*
    Stats.BestCount := BestLines[0].Count;
    Stats.PixelCount := PageWidth * PageHeight;
    Stats.AccumulatorSize := AccumulatorSize;
    Stats.AccumulatedCounts := AccumulatedCounts;
    Stats.TestedPixels := AccumulatedCounts div AlphaSteps;
  *)
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
  i, w, h: Integer;
begin
  h := length(raster);
  if h > 0 then
      w := length(raster[0])
  else
      w := 0;
  stream.write(w, 4);
  stream.write(h, 4);
  for i := 0 to h - 1 do
      stream.write(raster[i, 0], w);
end;

procedure LoadByteRasterFromStream(var raster: TByteRaster; stream: TCoreClassStream);
var
  i, w, h: Integer;
begin
  stream.read(w, 4);
  stream.read(h, 4);
  SetLength(raster, h, w);
  for i := 0 to h - 1 do
      stream.read(raster[i, 0], w);
end;

procedure SaveWordRasterToStream(raster: TWordRaster; stream: TCoreClassStream);
var
  i, w, h: Integer;
begin
  h := length(raster);
  if h > 0 then
      w := length(raster[0])
  else
      w := 0;
  stream.write(w, 4);
  stream.write(h, 4);
  for i := 0 to h - 1 do
      stream.write(raster[i, 0], w * 2);
end;

procedure LoadWordRasterFromStream(var raster: TWordRaster; stream: TCoreClassStream);
var
  i, w, h: Integer;
begin
  stream.read(w, 4);
  stream.read(h, 4);
  SetLength(raster, h, w);
  for i := 0 to h - 1 do
      stream.read(raster[i, 0], w * 2);
end;
