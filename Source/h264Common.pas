{ ****************************************************************************** }
{ * h264Common.pas        by qq600585                                          * }
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

unit h264Common;

{$INCLUDE zDefine.inc}
{$POINTERMATH ON}

interface

uses
  h264Types, h264Stats, FPCGenericStructlist, CoreClasses, MemoryRaster, Math;

const
  SLICE_P = 5;
  SLICE_I = 7;

  MB_I_4x4 = 0;
  MB_I_16x16 = 1;
  MB_P_16x16 = 2;
  MB_P_SKIP = 3;
  MB_I_PCM = 4;

  INTRA_PRED_TOP = 0;
  INTRA_PRED_LEFT = 1;
  INTRA_PRED_DC = 2;
  INTRA_PRED_PLANE = 3; // I16x16
  INTRA_PRED_DDL = 3;   // I4x4
  INTRA_PRED_DDR = 4;
  INTRA_PRED_VR = 5;
  INTRA_PRED_HD = 6;
  INTRA_PRED_VL = 7;
  INTRA_PRED_HU = 8;
  INTRA_PRED_NA = 255;

  INTRA_PRED_CHROMA_DC = 0;
  INTRA_PRED_CHROMA_LEFT = 1;
  INTRA_PRED_CHROMA_TOP = 2;
  INTRA_PRED_CHROMA_PLANE = 3;

  NZ_COEF_CNT_NA = 255;

  EG_MAX_ABS = 2047; // = 2^12 / 2 - 1 (abs.maximum for exp-golomb encoding)
  MB_SKIP_MAX = EG_MAX_ABS * 2;

  { ordering of 8x8 luma blocks
    1 | 2
    --+--
    3 | 4

    ordering of 4x4 luma blocks
    0 |  1 |  4 |  5
    ---+----+----+---
    2 |  3 |  6 |  7
    ---+----+----+---
    8 |  9 | 12 | 13
    ---+----+----+---
    10 | 11 | 14 | 15
  }
  block_offset4: array [0 .. 15] of uint8_t = (
    0, 4, 64, 68,
    8, 12, 72, 76,
    128, 132, 192, 196,
    136, 140, 200, 204
    );

  { ordering of 4x4 chroma blocks
    c0       c1
    0 | 1 |  | 0 | 1
    ---+--|  |-- +--
    2 | 3 |  | 2 | 3
  }
  block_offset_chroma: array [0 .. 3] of uint8_t = (
    0, 4,
    64, 68
    );

  block_dc_order: array [0 .. 15] of uint8_t = (0, 1, 4, 5, 2, 3, 6, 7, 8, 9, 12, 13, 10, 11, 14, 15);

function is_intra(const M: int32_t): Boolean; inline;
function is_inter(const M: int32_t): Boolean; inline;

type
  // motion vector
  TMotionvec = record
    x, y: int16_t;

{$IFNDEF FPC}
    // operator overloads
    class operator Equal(const a, b: TMotionvec): Boolean;
    class operator Add(const a, b: TMotionvec): TMotionvec;
    class operator Subtract(const a, b: TMotionvec): TMotionvec;
    class operator Multiply(const a: TMotionvec; multiplier: int32_t): TMotionvec;
    class operator Divide(const a: TMotionvec; Divisor: int32_t): TMotionvec;
{$ENDIF FPC}
  end;

  PMotionvec = ^TMotionvec;

  TMotionVectorList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TMotionvec>;

{$IFDEF FPC}
operator = (const a, b: TMotionvec): Boolean;
operator / (const a: TMotionvec; const Divisor: int32_t): TMotionvec;
operator * (const a: TMotionvec; const multiplier: int32_t): TMotionvec;
operator + (const a, b: TMotionvec): TMotionvec;
operator - (const a, b: TMotionvec): TMotionvec;
{$ENDIF FPC}

function XYToMVec(const x: int32_t; const y: int32_t): TMotionvec; inline;

const
  ZERO_MV: TMotionvec = (x: 0; y: 0);

type
  PFrame = ^TFrame;

  // residual block
  TBlock = record
    t0, t1, t1_signs: uint8_t;
    ncoef, nlevel: uint8_t;
    run_before: array [0 .. 15] of uint8_t;
    level: array [0 .. 15] of int16_t;
  end;

  // boundary strength
  TBSarray = array [0 .. 3, 0 .. 3] of uint8_t;

  // macroblock
  PMacroblock = ^TMacroblock;

  TMacroblock = record
    x, y: int32_t; // position
    mbtype: int32_t;
    qp, qpc: uint8_t;
    chroma_qp_offset: int8_t;

    i4_pred_mode: array [0 .. 23] of uint8_t;
    { intra prediction mode for luma 4x4 blocks
      0..15  - blocks from current mb
      16..19 - top mb bottom row
      20..23 - left mb right column
    }
    i16_pred_mode: int32_t;    // intra 16x16 pred mode
    chroma_pred_mode: int32_t; // chroma intra pred mode

    mvp, mv_skip, mv: TMotionvec; // mvs: predicted, skip, coded
    fref: PFrame;                 // reference frame selected for inter prediction
    ref: int32_t;                 // reference frame L0 index
    cbp: int32_t;                 // cpb bitmask: 0..3 luma, 4..5 chroma u/v

    // luma
    pfenc, pfdec, pfpred: uint8_p;
    pixels: uint8_p;     // original pixels
    pred: uint8_p;       // predicted pixels
    mcomp: uint8_p;      // motion-compensated pixels (maps to pred!)
    pixels_dec: uint8_p; // decoded pixels

    // chroma
    pfenc_c, pfdec_c, pfpred_c: array [0 .. 1] of uint8_p;
    pixels_c, pred_c, mcomp_c, pixels_dec_c: array [0 .. 1] of uint8_p;

    // coef arrays
    dct: array [0 .. 24] of int16_p; // 0-15 - luma, 16-23 chroma, 24 - luma DC
    chroma_dc: array [0 .. 1, 0 .. 3] of int16_t;
    Block: array [0 .. 26] of TBlock; // 0-24 as in dct, 25/26 chroma_dc u/v

    // cache for speeding up the prediction process
    intra_pixel_cache: array [0 .. 33] of uint8_t;
    { 0,17 - top left pixel
      1..16 - pixels from top row
      18..33 - pixels from left column
    }

    // non-zero coef count of surrounding blocks for I4x4/I16x16/chroma ac blocks
    nz_coef_cnt: array [0 .. 23] of uint8_t;
    nz_coef_cnt_chroma_ac: array [0 .. 1, 0 .. 7] of uint8_t;
    nz_coef_cnt_dc: uint8_t;

    // me
    L0_mvp: array [0 .. 15] of TMotionvec; // predicted mv for L0 refs
    score_skip, score_skip_uv: int32_t;
    residual_bits: int32_t;

    // loopfilter
    mba, mbb: PMacroblock;
    bS_vertical, bS_horizontal: TBSarray;

    // analysis
    bitcost: int32_t;
  end;

  // frame
  TFrame = record
    ftype: int32_t; // slice type

    qp: int32_t;             // fixed quant parameter
    Num: int32_t;            // frame number
    mbs: PMacroblock;        // frame macroblocks
    num_ref_frames: int32_t; // L0 reference picture count

    // img data
    w, h: int32_t;                                  // width, height
    w_cr, h_cr: int32_t;                            // chroma w&h
    pw, ph: int32_t;                                // padded w&h
    mbw, mbh: int32_t;                              // macroblock width, height
    mem: array [0 .. 5] of uint8_p;                 // allocated memory
    plane: array [0 .. 2] of uint8_p;               // image planes
    luma_mc: array [0 .. 3] of uint8_p;             // luma planes for hpel interpolated samples (none, h, v, h+v)
    luma_mc_qpel: array [0 .. 7] of uint8_p;        // plane pointers for qpel mc
    plane_dec: array [0 .. 2] of uint8_p;           // decoded image planes
    stride, stride_c: int32_t;                      // luma stride, chroma stride
    frame_mem_offset, frame_mem_offset_cr: int32_t; // padding to image offset in bytes
    blk_offset: array [0 .. 15] of int32_t;         // 4x4 block offsets
    blk_chroma_offset: array [0 .. 3] of int32_t;   // 4x4 chroma block offsets
    filter_hv_temp: int16_p;                        // temp storage for fir filter
    refs: array [0 .. 15] of PFrame;                // L0 reference list

    // mb-adaptive quant data
    aq_table: uint8_p; // qp table
    qp_avg: Single;    // average quant

    // bitstream buffer
    bs_buf: uint8_p;

    // stats
    stats: TFrameStats;
    estimated_framebits: int32_t;
    qp_adj: int32_t;
  end;

  IInterPredCostEvaluator = class
    procedure SetQP(qp: int32_t); virtual; abstract;
    procedure SetMVPredAndRefIdx(const mvp: TMotionvec; const idx: int32_t); virtual; abstract;
    function bitcost(const mv: TMotionvec): int32_t; virtual; abstract;
  end;

procedure YV12ToRaster(const luma_ptr, u_ptr, v_ptr: uint8_p; const w, h, stride, stride_cr: int32_t; const dest: TMemoryRaster; const forceITU_BT_709, lumaFull: Boolean); overload;
procedure RasterToYV12(const sour: TMemoryRaster; const luma_ptr, u_ptr, v_ptr: uint8_p; const w, h: int32_t); overload;

var
  lookup_table_CCIR_601_1, lookup_table_ITU_BT_709: array [0 .. 3] of int32_p;

implementation

{$IFNDEF FPC}


class operator TMotionvec.Equal(const a, b: TMotionvec): Boolean;
begin
  Result := int32_t(a) = int32_t(b);
end;

class operator TMotionvec.Add(const a, b: TMotionvec): TMotionvec;
begin
  Result.x := a.x + b.x;
  Result.y := a.y + b.y;
end;

class operator TMotionvec.Subtract(const a, b: TMotionvec): TMotionvec;
begin
  Result.x := a.x - b.x;
  Result.y := a.y - b.y;
end;

class operator TMotionvec.Multiply(const a: TMotionvec; multiplier: int32_t): TMotionvec;
begin
  Result.x := a.x * multiplier;
  Result.y := a.y * multiplier;
end;

class operator TMotionvec.Divide(const a: TMotionvec; Divisor: int32_t): TMotionvec;
begin
  Result.x := a.x div Divisor;
  Result.y := a.y div Divisor;
end;

{$ELSE}


operator = (const a, b: TMotionvec): Boolean; inline;
begin
  Result := int32_t(a) = int32_t(b);
end;

operator / (const a: TMotionvec; const Divisor: int32_t): TMotionvec;
begin
  Result.x := a.x div Divisor;
  Result.y := a.y div Divisor;
end;

operator * (const a: TMotionvec; const multiplier: int32_t): TMotionvec;
begin
  Result.x := a.x * multiplier;
  Result.y := a.y * multiplier;
end;

operator + (const a, b: TMotionvec): TMotionvec;
begin
  Result.x := a.x + b.x;
  Result.y := a.y + b.y;
end;

operator - (const a, b: TMotionvec): TMotionvec;
begin
  Result.x := a.x - b.x;
  Result.y := a.y - b.y;
end;
{$ENDIF FPC}


function XYToMVec(const x: int32_t; const y: int32_t): TMotionvec;
begin
  Result.x := x;
  Result.y := y;
end;

function is_intra(const M: int32_t): Boolean;
begin
  Result := M in [MB_I_4x4, MB_I_16x16, MB_I_PCM];
end;

function is_inter(const M: int32_t): Boolean;
begin
  Result := M in [MB_P_16x16, MB_P_SKIP];
end;

procedure YV12ToRaster(const luma_ptr, u_ptr, v_ptr: uint8_p; const w, h, stride, stride_cr: int32_t; const dest: TMemoryRaster; const forceITU_BT_709, lumaFull: Boolean);
// conversion works on 2x2 pixels at once, since they share chroma info
var
  nw, nh, y, x: int32_t;
  p, pu, pv, t: uint8_p;   // source plane ptrs
  d: int32_t;              // dest index for topleft pixel
  r0, r1, r2, r4: int32_t; // scaled yuv values for rgb calculation
  t0, t1, t2, t3: int32_p; // lookup table ptrs
  row1, row2: PRasterColorEntry;

  function Clip(c: int32_t): uint8_t; inline;
  begin
    Result := uint8_t(c);
    if c > 255 then
        Result := 255
    else if c < 0 then
        Result := 0;
  end;

begin
  nw := (w shr 1) * 2;
  nh := (h shr 1) * 2;
  dest.SetSize(nw, nh);

  if forceITU_BT_709 then
    begin
      t0 := lookup_table_ITU_BT_709[0];
      t1 := lookup_table_ITU_BT_709[1];
      t2 := lookup_table_ITU_BT_709[2];
      t3 := lookup_table_ITU_BT_709[3];
    end
  else
    begin
      t0 := lookup_table_CCIR_601_1[0];
      t1 := lookup_table_CCIR_601_1[1];
      t2 := lookup_table_CCIR_601_1[2];
      t3 := lookup_table_CCIR_601_1[3];
    end;

  p := luma_ptr;
  pu := u_ptr;
  pv := v_ptr;

  for y := 0 to dest.height shr 1 - 1 do
    begin
      row1 := PRasterColorEntry(dest.ScanLine[y * 2]);
      row2 := PRasterColorEntry(dest.ScanLine[y * 2 + 1]);

      for x := 0 to dest.width shr 1 - 1 do
        begin
          // row start relative index
          d := x * 2;
          r0 := t0[(pv + x)^]; // chroma
          r1 := t1[(pu + x)^] + t2[(pv + x)^];
          r2 := t3[(pu + x)^];
          t := p + d; // upper left luma

          // upper left/right luma
          r4 := t^;
          if lumaFull then
              r4 := Round((255 / 219) * (r4 - 16));
          row1[d].r := Clip(r4 + r0);
          row1[d].g := Clip(r4 + r1);
          row1[d].b := Clip(r4 + r2);
          row1[d].a := 255;

          r4 := (t + 1)^;
          if lumaFull then
              r4 := Round((255 / 219) * (r4 - 16));
          row1[d + 1].r := Clip(r4 + r0);
          row1[d + 1].g := Clip(r4 + r1);
          row1[d + 1].b := Clip(r4 + r2);
          row1[d + 1].a := 255;

          // lower left/right luma
          r4 := (t + stride)^;
          if lumaFull then
              r4 := Round((255 / 219) * (r4 - 16));
          row2[d].r := Clip(r4 + r0);
          row2[d].g := Clip(r4 + r1);
          row2[d].b := Clip(r4 + r2);
          row2[d].a := 255;

          r4 := (t + 1 + stride)^;
          if lumaFull then
              r4 := Round((255 / 219) * (r4 - 16));
          row2[d + 1].r := Clip(r4 + r0);
          row2[d + 1].g := Clip(r4 + r1);
          row2[d + 1].b := Clip(r4 + r2);
          row2[d + 1].a := 255;
        end;

      inc(p, stride * 2);
      inc(pu, stride_cr);
      inc(pv, stride_cr);
    end;
end;

procedure RasterToYV12(const sour: TMemoryRaster; const luma_ptr, u_ptr, v_ptr: uint8_p; const w, h: int32_t);
  function Clip(c: int32_t): uint8_t; inline;
  begin
    Result := uint8_t(c);
    if c > 255 then
        Result := 255
    else
      if c < 0 then
        Result := 0;
  end;

var
  nw, nh, i, j: int32_t;
  c: TRasterColorEntry;
  y, u, v, uu, vv, cv, nv, cu, nu: uint8_p;
  v01, v02, v11, v12, u01, u02, u11, u12: uint8_t;
begin
  nw := (w shr 1) * 2;
  nh := (h shr 1) * 2;

  y := luma_ptr;
  uu := GetMemory(nw * nh * 2);
  u := uu;
  vv := @(uu[nw * nh]);
  v := vv;

  for j := 0 to nh - 1 do
    for i := 0 to nw - 1 do
      begin
        c.BGRA := sour.Pixel[i, j];
        y^ := Clip(Trunc(0.256788 * c.r + 0.504129 * c.g + 0.097906 * c.b + 16));
        inc(y);
        u^ := Clip(Trunc(-0.148223 * c.r - 0.290993 * c.g + 0.439216 * c.b + 128));
        inc(u);
        v^ := Clip(Trunc(0.439216 * c.r - 0.367788 * c.g - 0.071427 * c.b + 128));
        inc(v);
      end;

  u := u_ptr;
  v := v_ptr;
  j := 0;
  while j < nh do
    begin
      cv := vv + j * nw;
      nv := vv + (j + 1) * nw;
      cu := uu + j * nw;
      nu := uu + (j + 1) * nw;

      i := 0;
      while i < nw do
        begin
          v01 := (cv + i)^;
          v02 := (cv + i + 1)^;
          v11 := (nv + i)^;
          v12 := (nv + i + 1)^;
          v^ := (v01 + v02 + v11 + v12) div 4;

          u01 := (cu + i)^;
          u02 := (cu + i + 1)^;
          u11 := (nu + i)^;
          u12 := (nu + i + 1)^;
          u^ := (u01 + u02 + u11 + u12) div 4;

          inc(v);
          inc(u);
          inc(i, 2);
        end;
      inc(j, 2);
    end;

  FreeMemory(uu);
end;

procedure BuildLut;
const
  UV_CSPC_CCIR_601_1: array [0 .. 3] of Single = (1.4020, -0.3441, -0.7141, 1.7720); // CCIR 601-1
  UV_CSPC_ITU_BT_709: array [0 .. 3] of Single = (1.5701, -0.1870, -0.4664, 1.8556); // ITU.BT-709
var
  c, j: int32_t;
  v: Single;
begin
  for c := 0 to 3 do
    begin
      lookup_table_CCIR_601_1[c] := GetMemory(256 * 4);
      lookup_table_ITU_BT_709[c] := GetMemory(256 * 4);
    end;

  for c := 0 to 255 do
    begin
      v := c - 128;
      for j := 0 to 3 do
        begin
          lookup_table_CCIR_601_1[j, c] := Round(v * UV_CSPC_CCIR_601_1[j]);
          lookup_table_ITU_BT_709[j, c] := Round(v * UV_CSPC_ITU_BT_709[j]);
        end;
    end;
end;

procedure FreeLut;
var
  c: int32_t;
begin
  for c := 0 to 3 do
    begin
      FreeMemory(lookup_table_CCIR_601_1[c]);
      lookup_table_CCIR_601_1[c] := nil;

      FreeMemory(lookup_table_ITU_BT_709[c]);
      lookup_table_ITU_BT_709[c] := nil;
    end;
end;

initialization

BuildLut;

finalization

FreeLut;

end.
