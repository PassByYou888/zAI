{ ****************************************************************************** }
{ * h264VLC.pas        by qq600585                                             * }
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

unit h264VLC;

{$INCLUDE zDefine.inc}
{$POINTERMATH ON}

interface

uses
  h264Types, h264Common, h264BitStream, h264tables;

type
  residual_type_t = (RES_LUMA, RES_LUMA_DC, RES_LUMA_AC, RES_DC, RES_AC_U, RES_AC_V);

procedure vlc_init();
procedure vlc_done();

procedure cavlc_encode(const mb: TMacroblock; const blok: TBlock; const blk_idx: uint8_t; const res: residual_type_t; var bs: TBitStreamWriter);
function cavlc_block_bits(const mb: TMacroblock; const blok: TBlock; const blk_idx: uint8_t; const res: residual_type_t): int32_t;
procedure cavlc_analyse_block(var Block: TBlock; dct_coefs: int16_p; const ncoef: int32_t);

procedure write_se_code(var bs: TBitStreamWriter; n: int32_t);
procedure write_ue_code(var bs: TBitStreamWriter; const n: int32_t);
function se_code_len(n: int32_t): int32_t;
function ue_code_len(const n: int32_t): int32_t;

implementation

const
  // zigzag index position table
  zigzag_pos: array [0 .. 15] of uint8_t = (
    0, 1, 4, 8,
    5, 2, 3, 6,
    9, 12, 13, 10,
    7, 11, 14, 15
    );
  { zigzag16 pattern
    0:    1  2  6  7
    4:    3  5  8 13
    8:    4  9 12 14
    12:  10 11 15 16
  }

var
  ue_code_length_table: uint8_p;
  se_code_length_table: uint8_p;

const
  // maximum number (in absolute value) that can be exp-golomb encoded
  VLC_MAX_INT  = EG_MAX_ABS;
  VLC_TAB_SIZE = VLC_MAX_INT * 2 + 1;

  (* ******************************************************************************
    calculate exp-golomb vlc code length table
  *)
procedure vlc_init();
var
  Bits, n, Min, Max: int32_t;
begin
  ue_code_length_table := GetMemory(VLC_TAB_SIZE);
  se_code_length_table := GetMemory(VLC_TAB_SIZE);
  inc(se_code_length_table, VLC_MAX_INT);

  Min := 1;
  Max := 2;
  for Bits := 1 to 12 do
    begin
      for n := Min to (Max - 1) do
          ue_code_length_table[n - 1] := Bits * 2 - 1;
      Min := Min shl 1;
      Max := Max shl 1;
    end;

  for n := -VLC_MAX_INT to VLC_MAX_INT do
    begin
      if n < 1 then
          se_code_length_table[n] := ue_code_length_table[-2 * n]
      else
          se_code_length_table[n] := ue_code_length_table[2 * n - 1];
    end;
end;

procedure vlc_done();
begin
  FreeMemory(ue_code_length_table);
  dec(se_code_length_table, VLC_MAX_INT);
  FreeMemory(se_code_length_table);
end;

(* ******************************************************************************
  vlc writing: signed and unsigned exp-golomb codes
*)
procedure write_se_code(var bs: TBitStreamWriter; n: int32_t);
begin
  if n < 1 then
      n := -2 * n
  else
      n := 2 * n - 1;
  bs.write(n + 1, ue_code_length_table[n]);
end;

procedure write_ue_code(var bs: TBitStreamWriter; const n: int32_t);
begin
  bs.write(n + 1, ue_code_length_table[n]);
end;

function se_code_len(n: int32_t): int32_t;
begin
  Result := se_code_length_table[n];
end;

function ue_code_len(const n: int32_t): int32_t;
begin
  Result := ue_code_length_table[n];
end;

procedure zigzag16(a, b: int16_p);
begin
  a[0] := b[0];
  a[1] := b[1];
  a[2] := b[4];
  a[3] := b[8];
  a[4] := b[5];
  a[5] := b[2];
  a[6] := b[3];
  a[7] := b[6];
  a[8] := b[9];
  a[9] := b[12];
  a[10] := b[13];
  a[11] := b[10];
  a[12] := b[7];
  a[13] := b[11];
  a[14] := b[14];
  a[15] := b[15];
end;

procedure zigzag15(a, b: int16_p);
begin
  a[0] := b[0];
  a[1] := b[3];
  a[2] := b[7];
  a[3] := b[4];
  a[4] := b[1];
  a[5] := b[2];
  a[6] := b[5];
  a[7] := b[8];
  a[8] := b[11];
  a[9] := b[12];
  a[10] := b[9];
  a[11] := b[6];
  a[12] := b[10];
  a[13] := b[13];
  a[14] := b[14];
end;

// get table index according to nz counts of surrounding blocks
function predict_nz_count_to_tab(const nzc: array of uint8_t; const i: uint8_t; const chroma: Boolean): uint8_t;
const
  { values:
    0..15  - current mb index
    16..19 - top mb, bottom row
    20..23 - left mb, rightmost column

    index: 0 - top/a, 1 - left/b
  }
  idx: array [0 .. 15, 0 .. 1] of uint8_t = (
    (16, 20), (17, 0), (0, 21), (1, 2), (18, 1), (19, 4), (4, 3), (5, 6), (2, 22), (3, 8), (8, 23), (9, 10), (6, 9), (7, 12), (12, 11), (13, 14));
  { 0..3 - current
    4, 5 - top mb, lower row
    6, 7 - left mb, right column
  }
  idxc: array [0 .. 3, 0 .. 1] of uint8_t = ((4, 6), (5, 0), (0, 7), (1, 2));

  nz2tab: array [0 .. 16] of uint8_t = (0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3);
var
  a, b, NC: uint8_t;
begin
  if not chroma then
    begin
      a := nzc[idx[i, 0]];
      b := nzc[idx[i, 1]];
    end
  else
    begin
      a := nzc[idxc[i, 0]];
      b := nzc[idxc[i, 1]];
    end;
  if (a = NZ_COEF_CNT_NA) and (b = NZ_COEF_CNT_NA) then
      NC := 0
  else if a = NZ_COEF_CNT_NA then
      NC := b
  else if b = NZ_COEF_CNT_NA then
      NC := a
  else
      NC := (a + b + 1) shr 1;

  Result := nz2tab[NC];
end;

{
  9.2.2 Parsing process for level information
  7 tables for sufflen range 0..6 according to JVT-E146 / JVT-D034 - sample code for VLC1-6
}
procedure encode_level(var bs: TBitStreamWriter; const level, suflen: int32_t);
var
  Code: int32_t;
  levabs, Sign, Shift, escape, prefix, suffix, length, Bits: int32_t;
begin
  // Lev-VLC 0
  if suflen = 0 then
    begin
      Code := (Abs(level) - 1) * 2;
      if level < 0 then
          inc(Code);
      if Code < 14 then
        begin
          bs.write(1, Code + 1); // abs(1..7)
        end
      else
        begin
          if Code < 30 then
            begin
              bs.write(1, 15); // escape short - abs(8..15)
              bs.write(Code - 14, 4);
            end
          else
            begin
              bs.write(1, 16); // escape long - abs(16..)
              bs.write(Code - 30, 12);
            end;
        end;
    end
  else
    begin
      // Lev-VLC 1-6
      levabs := Abs(level);
      Sign := (level shr 31) and 1;
      Shift := suflen - 1;
      escape := (15 shl Shift) + 1;
      prefix := (levabs - 1) shr Shift;
      suffix := (levabs - 1) - (prefix shl Shift);

      if levabs < escape then
        begin
          length := prefix + suflen + 1;
          Bits := (1 shl (Shift + 1)) or (suffix shl 1) or Sign;
        end
      else
        begin
          length := 28;
          Bits := (1 shl 12) or ((levabs - escape) shl 1) or Sign;
        end;

      bs.write(Bits, length);
    end;
end;

(* ******************************************************************************
  cavlc_encode
*)
procedure cavlc_encode
  (const mb: TMacroblock; const blok: TBlock; const blk_idx: uint8_t; const res: residual_type_t; var bs: TBitStreamWriter);
var
  i: int32_t;
  coef: int32_t;
  run_before, zeros_left, total_zeros: int32_t;
  Nz,              // TotalCoeff( coeff_token )
  t0, t1: int32_t; // trailing 0, TrailingOnes( coeff_token )
  t1_signs: int32_t;

  tab: uint8_t;
  suffix_length: uint8_t;
  vlc: vlc_bits_len;

begin
  t0 := blok.t0;
  t1 := blok.t1;
  t1_signs := blok.t1_signs;
  Nz := blok.nlevel;

  // coef_token
  if res <> RES_DC then
    begin
      case res of
        RES_LUMA, RES_LUMA_AC, RES_LUMA_DC: tab := predict_nz_count_to_tab(mb.nz_coef_cnt, blk_idx, False);
        RES_AC_U: tab := predict_nz_count_to_tab(mb.nz_coef_cnt_chroma_ac[0], blk_idx, True);
        RES_AC_V: tab := predict_nz_count_to_tab(mb.nz_coef_cnt_chroma_ac[1], blk_idx, True);
      end;
      bs.write(tab_coef_num[tab, Nz, t1][0], tab_coef_num[tab, Nz, t1][1])
    end
  else
      bs.write(tab_coef_num_chroma_dc[Nz, t1][0], tab_coef_num_chroma_dc[Nz, t1][1]);
  if Nz = 0 then
      Exit; // no coefs

  // trailing 1s signs
  for i := 0 to t1 - 1 do
      bs.write((t1_signs shr i) and 1);

  { 9.2.2 Parsing process for level information }
  // levels (nonzero coefs)
  if (Nz > 10) and (t1 < 3) then
      suffix_length := 1
  else
      suffix_length := 0;

  for i := t1 to Nz - 1 do
    begin
      coef := blok.level[i];

      // first coeff can't be |1| if t1 < 3, so we can code it as coeff lower by one
      if (i = t1) and (t1 < 3) then
        if coef > 0 then
            dec(coef)
        else
            inc(coef);

      encode_level(bs, coef, suffix_length);

      if suffix_length = 0 then
          suffix_length := 1;
      if (Abs(blok.level[i]) > (3 shl (suffix_length - 1))) and (suffix_length < 6) then
          inc(suffix_length);
    end;

  // total number of zeros in runs
  total_zeros := blok.ncoef - Nz - t0;
  if Nz < blok.ncoef then
    begin
      if res <> RES_DC then
        begin
          if Nz < 8 then
              vlc := tab_total_zeros0[Nz, total_zeros]
          else
              vlc := tab_total_zeros1[Nz, total_zeros];
        end
      else
          vlc := tab_total_zeros_chroma_dc[Nz, total_zeros];
      bs.write(vlc[0], vlc[1]);
    end;

  // run_before
  if total_zeros > 0 then
    begin
      zeros_left := total_zeros;
      for i := 0 to Nz - 2 do
        begin

          run_before := blok.run_before[i];
          if run_before < 7 then
            begin
              tab := zeros_left;
              if tab > 7 then
                  tab := 7;
              bs.write(tab_run_before[tab, run_before][0], tab_run_before[tab, run_before][1]);
            end
          else
              bs.write(1, run_before - 3);

          dec(zeros_left, run_before);
          if zeros_left <= 0 then
              Break;
        end;
    end;
end;

{ *******************************************************************************
  bitcost functions
}
function level_cost(const level, suflen: int32_t): int32_t;
var
  Code: int32_t;
  levabs, Shift, escape, prefix: int32_t;
begin
  Result := 0;
  // Lev-VLC 0
  if suflen = 0 then
    begin
      Code := (Abs(level) - 1) * 2;
      if level < 0 then
          inc(Code);
      if Code < 14 then
        begin
          Result := Code + 1; // abs(1..7)
        end
      else
        begin
          if Code < 30 then
            begin
              Result := 19;
            end
          else
            begin
              Result := 28;
            end;
        end;
    end
  else
    begin
      // Lev-VLC 1-6
      levabs := Abs(level);
      Shift := suflen - 1;
      escape := (15 shl Shift) + 1;
      prefix := (levabs - 1) shr Shift;
      if levabs < escape then
          Result := prefix + suflen + 1
      else
          Result := 28;
    end;
end;

function cavlc_block_bits(const mb: TMacroblock; const blok: TBlock; const blk_idx: uint8_t; const res: residual_type_t): int32_t;
var
  i: int32_t;
  coef: int32_t;
  run_before, zeros_left, total_zeros: int32_t;
  Nz: int32_t;     // TotalCoeff( coeff_token )
  t0, t1: int32_t; // trailing 0, TrailingOnes( coeff_token )
  tab: uint8_t;
  suffix_length: uint8_t;
  vlc: vlc_bits_len;
begin
  Result := 0;
  t0 := blok.t0;
  t1 := blok.t1;
  Nz := blok.nlevel;

  // coef_token
  if res <> RES_DC then
    begin
      case res of
        RES_LUMA, RES_LUMA_AC, RES_LUMA_DC: tab := predict_nz_count_to_tab(mb.nz_coef_cnt, blk_idx, False);
        RES_AC_U: tab := predict_nz_count_to_tab(mb.nz_coef_cnt_chroma_ac[0], blk_idx, True);
        RES_AC_V: tab := predict_nz_count_to_tab(mb.nz_coef_cnt_chroma_ac[1], blk_idx, True);
      end;
      inc(Result, tab_coef_num[tab, Nz, t1][1]);
    end
  else
      inc(Result, tab_coef_num_chroma_dc[Nz, t1][1]);
  if Nz = 0 then
      Exit; // no coefs

  // trailing 1s signs
  inc(Result, t1);

  { 9.2.2 Parsing process for level information }
  // levels (nonzero coefs)
  if (Nz > 10) and (t1 < 3) then
      suffix_length := 1
  else
      suffix_length := 0;

  for i := t1 to Nz - 1 do
    begin
      coef := blok.level[i];

      // first coeff can't be |1| if t1 < 3, so we can code it as coeff lower by one
      if (i = t1) and (t1 < 3) then
        if coef > 0 then
            dec(coef)
        else
            inc(coef);

      inc(Result, level_cost(coef, suffix_length));

      if suffix_length = 0 then
          suffix_length := 1;
      if (Abs(blok.level[i]) > (3 shl (suffix_length - 1))) and (suffix_length < 6) then
          inc(suffix_length);
    end;

  // total number of zeros in runs
  total_zeros := blok.ncoef - Nz - t0;
  if Nz < blok.ncoef then
    begin
      if res <> RES_DC then
        begin
          if Nz < 8 then
              vlc := tab_total_zeros0[Nz, total_zeros]
          else
              vlc := tab_total_zeros1[Nz, total_zeros];
        end
      else
          vlc := tab_total_zeros_chroma_dc[Nz, total_zeros];
      inc(Result, vlc[1]);
    end;

  // run_before
  if total_zeros > 0 then
    begin
      zeros_left := total_zeros;
      for i := 0 to Nz - 2 do
        begin

          run_before := blok.run_before[i];
          if run_before < 7 then
            begin
              tab := zeros_left;
              if tab > 7 then
                  tab := 7;
              inc(Result, tab_run_before[tab, run_before][1]);
            end
          else
              inc(Result, run_before - 3);

          dec(zeros_left, run_before);
          if zeros_left <= 0 then
              Break;
        end;
    end;
end;

// ******************************************************************************
procedure cavlc_analyse_block(var Block: TBlock; dct_coefs: int16_p; const ncoef: int32_t);
var
  i, t0, Zeros, n: int32_t;
  p: array [0 .. 15] of int16_t;
  coef: int32_t;
  count_t1: Boolean;
begin
  Block.t0 := 0;
  Block.t1 := 0;
  Block.nlevel := 0;
  Block.ncoef := ncoef;

  // zigzag16
  if ncoef = 4 then
    begin
      int64_p(@p)^ := int64_p(dct_coefs)^;
      if int64_p(@p)^ = 0 then
          Exit;
    end
  else
    begin
      if ncoef = 16 then
          zigzag16(@p[0], dct_coefs)
      else
          zigzag15(@p[0], dct_coefs + 1);
    end;

  // trailing 0s
  t0 := 0;
  for i := ncoef - 1 downto 0 do
    if p[i] = 0 then
        inc(t0)
    else
        Break;
  if t0 = ncoef then
      Exit;
  Block.t0 := t0;

  // levels, run_before
  n := 0;
  Zeros := 0;
  Block.t1_signs := 0;
  count_t1 := True;

  for i := i downto 0 do
    begin
      coef := p[i];
      if coef = 0 then
        begin
          inc(Zeros); // increase run_before
        end
      else
        begin
          Block.level[n] := coef; // store coef, if it's a t1, then store the sign separately

          // trailing 1s
          if count_t1 and (Block.t1 < 3) and (Abs(coef) = 1) then
            begin
              if coef < 0 then
                  Block.t1_signs := Block.t1_signs or (1 shl Block.t1);
              inc(Block.t1);
            end
          else
              count_t1 := False;

          if n > 0 then
              Block.run_before[n - 1] := Zeros; // save run_before
          Zeros := 0;
          inc(n);
        end;
    end;
  Block.run_before[n - 1] := Zeros;
  Block.nlevel := n;
end;

(* ******************************************************************************
  ****************************************************************************** *)
initialization

vlc_init();

finalization

vlc_done();

end.
 
 
