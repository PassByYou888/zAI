{ ****************************************************************************** }
{ * h264Intra_pred.pas        by qq600585                                      * }
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

unit h264Intra_pred;

{$INCLUDE zDefine.inc}

interface

uses
  h264Types, h264Common, h264Util, h264Pixel, CoreClasses;

type
  TIntraPredictor = class
  private
    mbcmp_16x16, mbcmp_8x8, mbcmp_4x4: mbcmp_func_t;
    pred4_cache: array [0 .. 8] of uint8_p; // cache for 4x4 predicted pixels
  public
    // luma
    pixels, prediction: uint8_p;
    frame_stride: int32_t;
    // chroma
    pixels_c, prediction_c: array [0 .. 1] of uint8_p;
    stride_c: int32_t;
    pixel_cache: uint8_p;
    mb_width: int32_t;

    constructor Create;
    destructor Destroy; override;
    procedure UseSATDCompare;
    procedure Predict_4x4(Mode: int32_t; ref: uint8_p; mbx, mby, n: int32_t);
    procedure Predict_8x8_cr(Mode: int32_t; refU, refV: uint8_p; mbx, mby: int32_t);
    procedure Predict_16x16(Mode: int32_t; mbx, mby: int32_t);

    // Get best mode for i4x4 prediction. Also stores the predicted pixels
    function Analyse_4x4(const ref: uint8_p; const mbx, mby, n: int32_t): int32_t;

    procedure Analyse_8x8_cr(refU, refV: uint8_p; mbx, mby: int32_t; out Mode: int32_t);
    procedure Analyse_16x16(mbx, mby: int32_t; out Mode: int32_t; out score: int32_t);
  end;

  TPredict4x4Func = procedure(Src, Dst: uint8_p; stride: int32_t);
  TPredict16x16Func = procedure(Src, Dst: uint8_p);

var
  predict_top16, predict_left16, predict_plane16: TPredict16x16Func;

procedure intra_pred_init;

implementation

uses DoStatusIO;

const
  I4x4CACHE_STRIDE = 16;

procedure predict_top4(Src, Dst: uint8_p; sstride: int32_t);
var
  p: int32_t;
  i: int32_t;
begin
  dec(Src, sstride);
  p := int32_p(Src)^;
  for i := 0 to 3 do
    begin
      int32_p(Dst)^ := p;
      inc(Dst, I4x4CACHE_STRIDE);
    end;
end;

procedure predict_left4(Src, Dst: uint8_p; sstride: int32_t);
var
  i, p: int32_t;
begin
  dec(Src);
  for i := 0 to 3 do
    begin
      p := (Src^ shl 8) or Src^;
      int32_p(Dst)^ := (p shl 16) or p;
      inc(Src, sstride);
      inc(Dst, I4x4CACHE_STRIDE);
    end;
end;

procedure predict_dc4(Src, Dst: uint8_p; sstride: int32_t; mbx, mby, n: uint16_t);
var
  has_top, has_left: Boolean;
  DC, i, Shift: int32_t;
begin
  has_top := (mby > 0) or not(n in [0, 1, 4, 5]);
  has_left := (mbx > 0) or not(n in [0, 2, 8, 10]);
  DC := 0;
  Shift := 0;
  if has_top then
    begin
      for i := 0 to 3 do
          inc(DC, Src[i - sstride]);
      Shift := 2;
    end;
  if has_left then
    begin
      for i := 0 to 3 do
          inc(DC, Src[-1 + i * sstride]);
      inc(Shift, 2);
    end;

  if Shift = 4 then
      DC := (DC + 4) shr 3
  else if Shift = 2 then
      DC := (DC + 2) shr 2
  else
      DC := 128;
  DC := DC or (DC shl 8) or (DC shl 16) or (DC shl 24); // spread

  for i := 0 to 3 do
    begin
      int32_p(Dst)^ := DC;
      inc(Dst, I4x4CACHE_STRIDE);
    end;
end;

{
  8.3.1.2.4  Specification of Intra_4x4_Diagonal_Down_Left prediction mode
  If x is equal to 3 and y is equal to 3,
  pred4x4L[x, y] = ( p[6, -1] + 3 * p[7, -1] + 2 ) >> 2
  Otherwise (x is not equal to 3 or y is not equal to 3),
  pred4x4L[x, y] = ( p[x + y, -1] + 2 * p[x + y + 1, -1] + p[x + y + 2, -1] + 2 ) >> 2
}
procedure predict_ddl4(Src, Dst: uint8_p; sstride: int32_t);
var
  x, y: int32_t;
begin
  Src := Src - sstride;
  for y := 0 to 3 do
    for x := 0 to 3 do
        Dst[y * I4x4CACHE_STRIDE + x] := (Src[x + y]
        + Src[x + y + 1] * 2
        + Src[x + y + 2] + 2) shr 2;
  Dst[3 * I4x4CACHE_STRIDE + 3] := (Src[6] + 3 * Src[7] + 2) shr 2
end;

{
  8.3.1.2.5  Specification of Intra_4x4_Diagonal_Down_Right prediction mode
  If x is greater than y,
  pred4x4L[x, y] = ( p[x - y - 2, -1]   + 2 * p[x - y - 1, -1]  + p[x - y, -1] + 2 ) >> 2
  Otherwise if x is less than y,
  pred4x4L[x, y] = ( p[-1, y - x - 2]   + 2 * p[-1, y - x - 1]  + p[-1, y - x] + 2 ) >> 2
  Otherwise (x is equal to y),
  pred4x4L[x, y] = ( p[0, -1] + 2 * p[-1, -1] + p[-1, 0] + 2 ) >> 2
}
procedure predict_ddr4(Src, Dst: uint8_p; sstride: int32_t);
var
  x, y: int32_t;
begin
  for y := 0 to 3 do
    for x := 0 to 3 do
      if x > y then
          Dst[y * I4x4CACHE_STRIDE + x] := (Src[x - y - 2 - sstride]
          + Src[x - y - 1 - sstride] * 2
          + Src[x - y - sstride] + 2) shr 2
      else if x < y then
          Dst[y * I4x4CACHE_STRIDE + x] := (Src[-1 + (y - x - 2) * sstride]
          + Src[-1 + (y - x - 1) * sstride] * 2
          + Src[-1 + (y - x) * sstride] + 2) shr 2
      else { x = y }
          Dst[y * I4x4CACHE_STRIDE + x] := (Src[-sstride]
          + Src[-1 - sstride] * 2
          + Src[-1] + 2) shr 2
end;

(* vertical right *)
procedure predict_vr4(Src, Dst: uint8_p; stride: int32_t);
var
  z, x, y, i: int32_t;
begin
  for x := 0 to 3 do
    for y := 0 to 3 do
      begin
        z := 2 * x - y;
        if z >= 0 then
          begin
            i := x - (y div 2) - stride;
            if (z and 1) = 0 then
                Dst[x + y * I4x4CACHE_STRIDE] := (Src[i - 1]
                + Src[i] + 1) div 2
            else
                Dst[x + y * I4x4CACHE_STRIDE] := (Src[i - 2]
                + Src[i - 1] * 2
                + Src[i] + 2) div 4
          end
        else if z = -1 then
            Dst[x + y * I4x4CACHE_STRIDE] := (Src[-1] + Src[-1 - stride] * 2 + Src[-stride] + 2) div 4
        else
            Dst[x + y * I4x4CACHE_STRIDE] := (Src[-1 + (y - x - 1) * stride]
            + Src[-1 + (y - x - 2) * stride] * 2
            + Src[-1 + (y - x - 3) * stride] + 2) div 4;
      end;
end;

procedure predict_vl4(Src, Dst: uint8_p; stride: int32_t);
var
  x, y: int32_t;
begin
  for x := 0 to 3 do
    for y := 0 to 3 do
      if (y and 1) = 1 then
          Dst[x + y * I4x4CACHE_STRIDE] := (Src[x + (y div 2) - stride] + Src[x + (y div 2) + 1 - stride] * 2 + Src[x + (y div 2) + 2 - stride] + 2) div 4
      else
          Dst[x + y * I4x4CACHE_STRIDE] := (Src[x + (y div 2) - stride] + Src[x + (y div 2) + 1 - stride] + 1) div 2;
end;

procedure predict_hd4(Src, Dst: uint8_p; stride: int32_t);
var
  z, x, y, i: int32_t;
begin
  for x := 0 to 3 do
    for y := 0 to 3 do
      begin
        z := 2 * y - x;
        if z >= 0 then
          begin
            i := y - (x div 2);
            if (z and 1) = 0 then
                Dst[x + y * I4x4CACHE_STRIDE] := (Src[-1 + (i - 1) * stride] + Src[-1 + i * stride] + 1) div 2
            else
                Dst[x + y * I4x4CACHE_STRIDE] := (Src[-1 + (i - 2) * stride] + Src[-1 + (i - 1) * stride] * 2 + Src[-1 + i * stride] + 2) div 4
          end
        else if z = -1 then
            Dst[x + y * I4x4CACHE_STRIDE] := (Src[-1] + Src[-1 - stride] * 2 + Src[-stride] + 2) div 4
        else
          begin
            i := x - y - 1 - stride;
            Dst[x + y * I4x4CACHE_STRIDE] := (Src[i] + Src[i - 1] * 2 + Src[i - 2] + 2) div 4;
          end;
      end;
end;

{
  8.3.1.2.9 Specification of Intra_4x4_Horizontal_Up prediction mode
  zHU be set equal to x + 2 * y.
  - If zHU is equal to 0, 2, or 4
  pred4x4L[ x, y ] = ( p[ -1, y + ( x >> 1 ) ] + p[ -1, y + ( x >> 1 ) + 1 ] + 1 ) >> 1
  - Otherwise, if zHU is equal to 1 or 3
  pred4x4L[ x, y ] = ( p[ -1, y + ( x >> 1 ) ] + 2 * p[ -1, y + ( x >> 1 ) + 1 ] + p[ -1, y + ( x >> 1 ) + 2 ] + 2 ) >> 2
  - Otherwise, if zHU is equal to 5,
  pred4x4L[ x, y ] = ( p[ -1, 2 ] + 3 * p[ -1, 3 ] + 2 ) >> 2
  - Otherwise (zHU is greater than 5),
  pred4x4L[ x, y ] = p[ -1, 3 ]
}
procedure predict_hu4(Src, Dst: uint8_p; stride: int32_t);
var
  z, x, y, i: int32_t;
begin
  for x := 0 to 3 do
    for y := 0 to 3 do
      begin
        z := x + 2 * y;
        if (z >= 0) and (z < 5) then
          begin
            i := y + (x div 2);
            if (z and 1) = 0 then
                Dst[x + y * I4x4CACHE_STRIDE] := (Src[-1 + i * stride] + Src[-1 + (i + 1) * stride] + 1) shr 1
            else
                Dst[x + y * I4x4CACHE_STRIDE] := (Src[-1 + i * stride] + Src[-1 + (i + 1) * stride] * 2 + Src[-1 + (i + 2) * stride] + 2) shr 2
          end
        else if z = 5 then
            Dst[x + y * I4x4CACHE_STRIDE] := (Src[-1 + 2 * stride] + Src[-1 + 3 * stride] * 3 + 2) shr 2
        else
            Dst[x + y * I4x4CACHE_STRIDE] := Src[-1 + 3 * stride];
      end;
end;

(* ******************************************************************************
  8.3.2 Intra_16x16 prediction process for luma samples
  ****************************************************************************** *)
procedure predict_top16_pas(Src, Dst: uint8_p);
var
  p1, p2: Int64_t;
  i: int32_t;
begin
  p1 := int64_p(Src + 1)^;
  p2 := int64_p(Src + 9)^;
  for i := 0 to 7 do
    begin
      int64_p(Dst)^ := p1;
      int64_p(Dst + 8)^ := p2;
      int64_p(Dst + 16)^ := p1;
      int64_p(Dst + 24)^ := p2;
      inc(Dst, 32);
    end;
end;

procedure predict_left16_pas(Src, Dst: uint8_p);
var
  i: int32_t;
  v: Int64_t;
begin
  inc(Src, 18);
  for i := 0 to 15 do
    begin
      v := (Src^ shl 24) or (Src^ shl 16) or (Src^ shl 8) or Src^;
      v := v or (v shl 32);
      int64_p(Dst)^ := v;
      int64_p(Dst + 8)^ := v;
      inc(Src);
      inc(Dst, 16);
    end;
end;

procedure predict_dc16(Src, Dst: uint8_p; const mbx, mby: uint16_t);
var
  DC, i, avail: int32_t;
begin
  DC := 0;
  avail := 0;
  if mby > 0 then
    begin
      for i := 1 to 16 do
          inc(DC, Src[i]);
      inc(avail);
    end;
  if mbx > 0 then
    begin
      for i := 18 to 33 do
          inc(DC, Src[i]);
      inc(avail);
    end;

  if avail = 2 then
      DC := (DC + 16) shr 5
  else if avail = 1 then
      DC := (DC + 8) shr 4
  else
      DC := 128;

  FillPtrByte(Dst, 256, uint8_t(DC));
end;

procedure predict_plane16_pas(Src, Dst: uint8_p);
var
  x, y: int32_t;
  a, b, c, d, h, v, i: int32_t;

begin
  h := 0;
  v := 0;
  inc(Src);

  for i := 0 to 7 do
    begin
      inc(h, (i + 1) * (Src[8 + i] - Src[6 - i]));
      inc(v, (i + 1) * (Src[17 + 8 + i] - Src[17 + 6 - i]));
    end;

  a := 16 * (Src[15 + 17] + Src[15]) + 16;
  b := SAR16(SmallInt(5 * h + 32), 6);
  c := SAR16(SmallInt(5 * v + 32), 6);

  for y := 0 to 15 do
    begin
      d := a + c * (y - 7);
      for x := 0 to 15 do
        begin
          i := b * (x - 7) + d;
          if i < 0 then
              Dst[x] := 0
          else
            begin
              i := i shr 5;
              if i > 255 then
                  i := 255;
              Dst[x] := uint8_t(i);
            end;
        end;
      inc(Dst);
    end;
end;

(* ******************************************************************************
  8.3.3 Intra prediction process for chroma samples
  ****************************************************************************** *)
procedure predict_dc8(Src, Dst: uint8_p; sstride: int32_t; mbx, mby: uint16_t);
var
  has_top, has_left: Boolean;
  i, k: int32_t;
  DC, Shift: int32_t;
  dcf: array [0 .. 3] of uint8_t;

begin
  has_top := mby > 0;
  has_left := mbx > 0;

  // 0
  DC := 0;
  Shift := 0;
  if has_top then
    begin
      for i := 0 to 3 do
          inc(DC, Src[i - sstride]);
      Shift := 2;
    end;
  if has_left then
    begin
      for i := 0 to 3 do
          inc(DC, Src[-1 + i * sstride]);
      inc(Shift, 2);
    end;
  if Shift = 4 then
      DC := (DC + 4) shr 3
  else if Shift = 2 then
      DC := (DC + 2) shr 2
  else
      DC := 128;
  dcf[0] := DC;

  // 1
  DC := 0;
  if has_top then
    begin
      for i := 0 to 3 do
          inc(DC, Src[4 + i - sstride]);
      DC := (DC + 2) shr 2;
    end
  else if has_left then
    begin
      for i := 0 to 3 do
          inc(DC, Src[-1 + i * sstride]);
      DC := (DC + 2) shr 2;
    end
  else
      DC := 128;
  dcf[1] := DC;

  // 2
  DC := 0;
  if has_left then
    begin
      for i := 0 to 3 do
          inc(DC, Src[-1 + (i + 4) * sstride]);
      DC := (DC + 2) shr 2;
    end
  else if has_top then
    begin
      for i := 0 to 3 do
          inc(DC, Src[i - sstride]);
      DC := (DC + 2) shr 2;
    end
  else
      DC := 128;
  dcf[2] := DC;

  // 3
  DC := 0;
  Shift := 0;
  if has_top then
    begin
      for i := 0 to 3 do
          inc(DC, Src[4 + i - sstride]);
      Shift := 2;
    end;
  if has_left then
    begin
      for i := 0 to 3 do
          inc(DC, Src[-1 + (4 + i) * sstride]);
      inc(Shift, 2);
    end;
  if Shift = 4 then
      DC := (DC + 4) shr 3
  else if Shift = 2 then
      DC := (DC + 2) shr 2
  else
      DC := 128;
  dcf[3] := DC;

  // write
  for i := 0 to 3 do
    begin
      for k := 0 to 3 do
          Dst[k] := dcf[0];
      for k := 4 to 7 do
          Dst[k] := dcf[1];
      inc(Dst, 16);
    end;

  for i := 0 to 3 do
    begin
      for k := 0 to 3 do
          Dst[k] := dcf[2];
      for k := 4 to 7 do
          Dst[k] := dcf[3];
      inc(Dst, 16);
    end;
end;

procedure predict_top8(Src, Dst: uint8_p; sstride: int32_t);
var
  p: Int64_t;
  i: int32_t;
begin
  dec(Src, sstride);
  p := int64_p(Src)^;
  for i := 0 to 7 do
    begin
      int64_p(Dst)^ := p;
      inc(Dst, 16);
    end;
end;

procedure predict_left8(Src, Dst: uint8_p; sstride: int32_t);
var
  i, j: int32_t;
begin
  dec(Src);
  for i := 0 to 7 do
    begin
      for j := 0 to 7 do
          Dst[j] := Src^;
      inc(Src, sstride);
      inc(Dst, 16);
    end;
end;

procedure predict_plane8(Src, Dst: uint8_p; stride: int32_t);
var
  x, y: int32_t;
  a, b, c, d, h, v, i: int32_t;

begin
  h := 0;
  v := 0;

  for x := 0 to 3 do
      inc(h, (x + 1) * (Src[-stride + 4 + x] - Src[-stride + 2 - x]));
  for y := 0 to 3 do
      inc(v, (y + 1) * (Src[(4 + y) * stride - 1] - Src[(2 - y) * stride - 1]));

  a := 16 * (Src[7 * stride - 1] + Src[-stride + 7]) + 16;
  b := SAR16(17 * h + 16, 5);
  c := SAR16(17 * v + 16, 5);

  for y := 0 to 7 do
    begin
      d := a + c * (y - 3);
      for x := 0 to 7 do
        begin
          i := b * (x - 3) + d;
          if i < 0 then
              Dst[x] := 0
          else
            begin
              i := i shr 5;
              if i > 255 then
                  i := 255;
              Dst[x] := uint8_t(i);
            end;
        end;
      inc(Dst, 16);
    end;
end;

const
  Predict4x4Funcs: array [INTRA_PRED_TOP .. INTRA_PRED_HU] of TPredict4x4Func = (
{$IFDEF FPC}@{$ENDIF FPC}predict_top4,
{$IFDEF FPC}@{$ENDIF FPC}predict_left4,
    nil, // INTRA_PRED_DC is different
{$IFDEF FPC}@{$ENDIF FPC}predict_ddl4,
{$IFDEF FPC}@{$ENDIF FPC}predict_ddr4,
{$IFDEF FPC}@{$ENDIF FPC}predict_vr4,
{$IFDEF FPC}@{$ENDIF FPC}predict_hd4,
{$IFDEF FPC}@{$ENDIF FPC}predict_vl4,
{$IFDEF FPC}@{$ENDIF FPC}predict_hu4);

constructor TIntraPredictor.Create;
var
  i: int32_t;
begin
  inherited Create;
  mbcmp_16x16 := DSP.sad_16x16;
  mbcmp_8x8 := DSP.sad_8x8;
  mbcmp_4x4 := DSP.sad_4x4;
  pred4_cache[0] := fev_malloc(9 * 4 * I4x4CACHE_STRIDE);
  for i := 1 to 8 do
      pred4_cache[i] := pred4_cache[i - 1] + 4 * I4x4CACHE_STRIDE;
end;

destructor TIntraPredictor.Destroy;
begin
  fev_free(pred4_cache[0]);
  inherited Destroy;
end;

procedure TIntraPredictor.UseSATDCompare;
begin
  mbcmp_16x16 := DSP.satd_16x16;
  mbcmp_8x8 := DSP.satd_8x8;
  mbcmp_4x4 := DSP.satd_4x4;
end;

procedure TIntraPredictor.Predict_4x4(Mode: int32_t; ref: uint8_p; mbx, mby, n: int32_t);
begin
  if Mode = INTRA_PRED_DC then
      predict_dc4(ref, prediction + block_offset4[n], frame_stride, mbx, mby, n)
  else
      Predict4x4Funcs[Mode](ref, prediction + block_offset4[n], frame_stride);
end;

procedure TIntraPredictor.Predict_8x8_cr(Mode: int32_t; refU, refV: uint8_p; mbx, mby: int32_t);
begin
  case Mode of
    INTRA_PRED_CHROMA_DC:
      begin
        predict_dc8(refU, prediction_c[0], stride_c, mbx, mby);
        predict_dc8(refV, prediction_c[1], stride_c, mbx, mby);
      end;
    INTRA_PRED_CHROMA_TOP:
      begin
        predict_top8(refU, prediction_c[0], stride_c);
        predict_top8(refV, prediction_c[1], stride_c);
      end;
    INTRA_PRED_CHROMA_LEFT:
      begin
        predict_left8(refU, prediction_c[0], stride_c);
        predict_left8(refV, prediction_c[1], stride_c);
      end;
    INTRA_PRED_CHROMA_PLANE:
      begin
        predict_plane8(refU, prediction_c[0], stride_c);
        predict_plane8(refV, prediction_c[1], stride_c);
      end
    else
      DoStatus('mb_intra_pred_chroma error: unknown predict mode');
  end;
end;

procedure TIntraPredictor.Predict_16x16(Mode: int32_t; mbx, mby: int32_t);
begin
  case Mode of
    INTRA_PRED_DC: predict_dc16(pixel_cache, prediction, mbx, mby);
    INTRA_PRED_TOP: predict_top16(pixel_cache, prediction);
    INTRA_PRED_LEFT: predict_left16(pixel_cache, prediction);
    INTRA_PRED_PLANE: predict_plane16(pixel_cache, prediction);
    else
      DoStatus('mb_intra_pred_16 error: unknown predict mode');
  end;
end;

function TIntraPredictor.Analyse_4x4(const ref: uint8_p; const mbx, mby, n: int32_t): int32_t;
const
  TopMask = 65484;        { !(n in [0, 1, 4, 5]) }
  LeftMask = 64250;       { !(n in [0, 2, 8, 10]) }
  TopLeftMask = 64200;    { n in [3, 6, 7, 9, 11, 12, 13, 14, 15] }
  InsideTTRMask = 22340;  { top/topright,      n in [2, 6, 8, 9, 10, 12, 14] }
  InsideLTTRMask = 21056; { left/top/topright, n in [6, 9, 12, 14] }
  OutsideTTRMask = 22391; { top/topright,      !(n in [3, 7, 11, 13, 15]) }
var
  pix: uint8_p;
  modes, Mode: int32_t;
  score, min_score: int32_t;
  Mask: int32_t;
  has_top, has_left, has_tl, has_inside_ttr, has_inside_lttr, has_outside_ttr: Boolean;
begin
  pix := pixels + block_offset4[n];

  // always run dc
  predict_dc4(ref, pred4_cache[INTRA_PRED_DC], frame_stride, mbx, mby, n);
  min_score := mbcmp_4x4(pix, pred4_cache[INTRA_PRED_DC], I4x4CACHE_STRIDE);
  Result := INTRA_PRED_DC;
  modes := 0;

  // rules based on the 4x4 block position inside 16x16 macroblock
  Mask := 1 shl n;
  has_top := (TopMask and Mask) > 0;
  has_left := (LeftMask and Mask) > 0;
  has_tl := (TopLeftMask and Mask) > 0;
  has_inside_ttr := (InsideTTRMask and Mask) > 0;
  has_inside_lttr := (InsideLTTRMask and Mask) > 0;
  has_outside_ttr := (OutsideTTRMask and Mask) > 0;

  // enable modes that need:
  // top pixels
  if (mby > 0) or has_top then
      modes := modes or (1 shl INTRA_PRED_TOP);
  // left pixels
  if (mbx > 0) or has_left then
      modes := modes or (1 shl INTRA_PRED_LEFT) or (1 shl INTRA_PRED_HU);
  // top & left pixels
  if ((mbx > 0) and (mby > 0)) or has_tl then
      modes := modes or (1 shl INTRA_PRED_DDR) or (1 shl INTRA_PRED_VR) or (1 shl INTRA_PRED_HD);
  // top & top-right pixels
  if ((mby > 0) and (mbx < mb_width - 1) and has_outside_ttr) or has_inside_ttr then
      modes := modes or (1 shl INTRA_PRED_DDL);
  // left, top & top-right pixels
  if ((mby > 0) and (mbx > 0) and (mbx < mb_width - 1) and has_outside_ttr) or has_inside_lttr then
      modes := modes or (1 shl INTRA_PRED_VL);

  // run all enabled modes
  for Mode := 0 to 8 do
    begin
      if ((1 shl Mode) and modes) > 0 then
        begin
          Predict4x4Funcs[Mode](ref, pred4_cache[Mode], frame_stride);
          score := mbcmp_4x4(pix, pred4_cache[Mode], I4x4CACHE_STRIDE);
          if score < min_score then
            begin
              min_score := score;
              Result := Mode;
            end;
        end;
    end;

  // restore best mode's prediction from cache
  pixel_load_4x4(prediction + block_offset4[n], pred4_cache[Result], I4x4CACHE_STRIDE);
end;

procedure TIntraPredictor.Analyse_8x8_cr(refU, refV: uint8_p; mbx, mby: int32_t; out Mode: int32_t);
var
  mscore, cscore: int32_t;
  cmp: mbcmp_func_t;

  procedure ipmode(M: uint8_t);
  begin
    cscore := cmp(pixels_c[0], prediction_c[0], 16);
    inc(cscore, cmp(pixels_c[1], prediction_c[1], 16));
    if cscore < mscore then
      begin
        Mode := M;
        mscore := cscore;
      end;
  end;

begin
  mscore := MaxInt;
  cmp := mbcmp_8x8;

  // dc
  predict_dc8(refU, prediction_c[0], stride_c, mbx, mby);
  predict_dc8(refV, prediction_c[1], stride_c, mbx, mby);
  ipmode(INTRA_PRED_CHROMA_DC);

  // top - vertical
  if (mby > 0) then
    begin
      predict_top8(refU, prediction_c[0], stride_c);
      predict_top8(refV, prediction_c[1], stride_c);
      ipmode(INTRA_PRED_CHROMA_TOP);
    end;

  // left - horizontal
  if (mbx > 0) then
    begin
      predict_left8(refU, prediction_c[0], stride_c);
      predict_left8(refV, prediction_c[1], stride_c);
      ipmode(INTRA_PRED_CHROMA_LEFT);
    end;

  // plane
  if (mbx > 0) and (mby > 0) then
    begin
      predict_plane8(refU, prediction_c[0], stride_c);
      predict_plane8(refV, prediction_c[1], stride_c);
      ipmode(INTRA_PRED_CHROMA_PLANE);
    end;

  // restore best mode
  Predict_8x8_cr(Mode, refU, refV, mbx, mby);
end;

procedure TIntraPredictor.Analyse_16x16(mbx, mby: int32_t; out Mode: int32_t; out score: int32_t);
var
  mscore, cscore: int32_t;
  cmp: mbcmp_func_t;

  procedure ipmode(M: uint8_t);
  begin
    cscore := cmp(pixels, prediction, 16);
    if cscore < mscore then
      begin
        Mode := M;
        mscore := cscore;
      end;
  end;

begin
  mscore := MaxInt;
  cmp := mbcmp_16x16;

  // dc
  predict_dc16(pixel_cache, prediction, mbx, mby);
  ipmode(INTRA_PRED_DC);

  // vertical
  if (mby > 0) then
    begin
      predict_top16(pixel_cache, prediction);
      ipmode(INTRA_PRED_TOP);
    end;

  // horizontal
  if (mbx > 0) then
    begin
      predict_left16(pixel_cache, prediction);
      ipmode(INTRA_PRED_LEFT);
    end;

  // plane
  if (mbx > 0) and (mby > 0) then
    begin
      predict_plane16(pixel_cache, prediction);
      ipmode(INTRA_PRED_PLANE);
    end;

  score := mscore;
end;

{$IF Defined(WIN64)}
{$L intra_pred_x64.obj}
procedure predict_top16_sse2(Src, Dst: uint8_p); external name 'predict_top16_sse2';
procedure predict_left16_ssse3(Src, Dst: uint8_p); external name 'predict_left16_ssse3';
procedure predict_plane16_sse2(Src, Dst: uint8_p); external name 'predict_plane16_sse2';
{$IFEND}

procedure intra_pred_init;
begin
  predict_top16 := @predict_top16_pas;
  predict_left16 := @predict_left16_pas;
  predict_plane16 := @predict_plane16_pas;

{$IF Defined(WIN64)}
      predict_top16   := @predict_top16_sse2;
      predict_plane16 := @predict_plane16_sse2;
      predict_left16  := @predict_left16_ssse3;
{$IFEND}
end;

end.
