{ ****************************************************************************** }
{ * h264Transquant.pas        by qq600585                                      * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit h264Transquant;

{$INCLUDE zDefine.inc}
{$POINTERMATH ON}

interface

uses h264Types, CoreClasses;

procedure transqt(Block: int16_p; const qp: uint8_t; const intra: Boolean; const quant_start_idx: uint8_t = 0);
procedure itransqt(Block: int16_p; const qp: uint8_t; const quant_start_idx: uint8_t = 0);

procedure transqt_dc_2x2(Block: int16_p; const qp: uint8_t);
procedure itransqt_dc_2x2(Block: int16_p; const qp: uint8_t);

procedure transqt_dc_4x4(Block: int16_p; const qp: uint8_t);
procedure itransqt_dc_4x4(Block: int16_p; const qp: uint8_t);

procedure itrans_dc(Block: int16_p);

implementation


const
  table_qp_div6: array [0 .. 51] of uint8_t =
    (0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8);
  table_qp_mod6: array [0 .. 51] of uint8_t =
    (0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3);

  { E matrix - scaling factors:
    a^2 = 1
    ab/2 = 2
    b^2/4 = 3
  }
  coef_idx: array [0 .. 15] of uint8_t = (
    1, 2, 1, 2,
    2, 3, 2, 3,
    1, 2, 1, 2,
    2, 3, 2, 3
    );

  // V = Qstep*PF*64 rescaling factor
  // -> LevelScale (8-252)
  table_v_coefs: array [0 .. 5, 1 .. 3] of uint8_t = (
    (10, 13, 16), // 0
    (11, 14, 18),
    (13, 16, 20),
    (14, 18, 23),
    (16, 20, 25),
    (18, 23, 29) // 5
    );

  // (PF/Qstep) mult. factor
  // 1, 2, 3
  // -> LevelScale2 (8-293)
  table_mf_coefs: array [0 .. 5, 1 .. 3] of int16_t = (
    (13107, 8066, 5243), // 0
    (11916, 7490, 4660),
    (10082, 6554, 4194),
    (9362, 5825, 3647),
    (8192, 5243, 3355),
    (7282, 4559, 2893) // 5
    );

type
  matrix_t    = array [0 .. 3, 0 .. 3] of int16_t;
  dc_matrix_t = array [0 .. 1, 0 .. 1] of int16_t;
  dc_matrix_p = ^dc_matrix_t;

var
  resc_factor,
    mult_factor: array [0 .. 5, 0 .. 15] of int16_t;

procedure init_tables;
var
  i, j: uint8_t;
begin
  for i := 0 to 5 do
    begin
      for j := 0 to 15 do
        begin
          mult_factor[i][j] := table_mf_coefs[i, coef_idx[j]];
          resc_factor[i][j] := table_v_coefs[i, coef_idx[j]];
        end;
    end;
end;

// Z = (|W| . MF + f) >> qbits
procedure quant(a: int16_p; const qp: uint8_t; const intra: Boolean; const sidx: uint8_t);
var
  i: int32_t;
  f: int32_t;
  qbits: uint8_t;
  MF: int16_p;
begin
  // multiply shift
  qbits := 15 + table_qp_div6[qp];
  // multiply factor
  MF := @mult_factor[table_qp_mod6[qp]];
  // rounding factor
  if intra then
      f := (1 shl qbits) div 3
  else
      f := (1 shl qbits) div 6;

  inc(a, sidx);
  inc(MF, sidx);
  for i := sidx to 15 do
    begin
      if a^ > 0 then
          a^ := (a^ * MF^ + f) shr qbits
      else
          a^ := -((f - a^ * MF^) shr qbits); // fix from x264
      inc(a);
      inc(MF);
    end;
end;

procedure core_4x4(Block: int16_p);
var
  M: matrix_t;
  E, f, g, h: array [0 .. 3] of int16_t;
  i: int32_t;
begin
  CopyPtr(Block, @M, 16 * 2);

  { aaaa
    bbbb
    cccc
    dddd
  }
  for i := 0 to 3 do
    begin
      E[i] := M[0][i] + M[3][i]; // a + d
      f[i] := M[0][i] - M[3][i]; // a - d
      g[i] := M[1][i] + M[2][i]; // b + c
      h[i] := M[1][i] - M[2][i]; // b - c
    end;

  for i := 0 to 3 do
    begin
      M[0][i] := E[i] + g[i];     // a + b +  c +  d
      M[1][i] := 2 * f[i] + h[i]; // 2a + b -  c - 2d
      M[2][i] := E[i] - g[i];     // a - b -  c +  d
      M[3][i] := f[i] - h[i] * 2; // a -2b + 2c -  d
    end;

  { abcd
    abcd
    abcd
    abcd
  }
  for i := 0 to 3 do
    begin
      E[i] := M[i][0] + M[i][3];
      f[i] := M[i][0] - M[i][3];
      g[i] := M[i][1] + M[i][2];
      h[i] := M[i][1] - M[i][2];
    end;

  for i := 0 to 3 do
    begin
      M[i][0] := E[i] + g[i];
      M[i][1] := 2 * f[i] + h[i];
      M[i][2] := E[i] - g[i];
      M[i][3] := f[i] - h[i] * 2;
    end;

  CopyPtr(@M, Block, 16 * 2);
end;

procedure transqt(Block: int16_p; const qp: uint8_t; const intra: Boolean; const quant_start_idx: uint8_t);
begin
  core_4x4(Block);
  quant(Block, qp, intra, quant_start_idx);
end;

(* ******************************************************************************
  iHCT + dequant
*)
procedure iquant(a: int16_p; const qp: uint8_t; const sidx: uint8_t);
var
  i: int32_t;
  Shift: int32_t;
  MF: int16_p;
begin
  Shift := table_qp_div6[qp];
  MF := @resc_factor[table_qp_mod6[qp]];
  inc(a, sidx);
  inc(MF, sidx);
  for i := sidx to 15 do
    begin
      a^ := a^ * MF^ shl Shift;
      inc(a);
      inc(MF);
    end;
end;

procedure icore_4x4(Block: int16_p);
var
  M: matrix_t;
  E, f, g, h: array [0 .. 3] of int16_t;
  i: int32_t;
begin
  CopyPtr(Block, @M, 16 * 2);

  for i := 0 to 3 do
    begin
      E[i] := M[i][0] + M[i][2];
      f[i] := M[i][0] - M[i][2];
      g[i] := M[i][1] + SAR16(M[i][3], 1);
      h[i] := SAR16(M[i][1], 1) - M[i][3];
    end;
  for i := 0 to 3 do
    begin
      M[i][0] := E[i] + g[i];
      M[i][1] := f[i] + h[i];
      M[i][2] := f[i] - h[i];
      M[i][3] := E[i] - g[i];
    end;

  for i := 0 to 3 do
    begin
      E[i] := M[0][i] + M[2][i];
      f[i] := M[0][i] - M[2][i];
      g[i] := M[1][i] + SAR16(M[3][i], 1);
      h[i] := SAR16(M[1][i], 1) - M[3][i];
    end;
  for i := 0 to 3 do
    begin
      M[0][i] := SAR16(E[i] + g[i] + 32, 6); // rescaling
      M[1][i] := SAR16(f[i] + h[i] + 32, 6);
      M[2][i] := SAR16(f[i] - h[i] + 32, 6);
      M[3][i] := SAR16(E[i] - g[i] + 32, 6);
    end;

  CopyPtr(@M, Block, 16 * 2);
end;

procedure itransqt(Block: int16_p; const qp: uint8_t; const quant_start_idx: uint8_t = 0);
begin
  iquant(Block, qp, quant_start_idx);
  icore_4x4(Block);
end;

(* ******************************************************************************
  chroma DC
*)
procedure trans_dc_2x2(Block: int16_p);
var
  M: dc_matrix_t;
  E, f, g, h: int32_t;
begin
  M := dc_matrix_p(Block)^;
  E := M[0, 0] + M[1, 0];
  f := M[0, 0] - M[1, 0];
  g := M[0, 1] + M[1, 1];
  h := M[0, 1] - M[1, 1];
  M[0, 0] := E + g;
  M[0, 1] := E - g;
  M[1, 0] := f + h;
  M[1, 1] := f - h;
  dc_matrix_p(Block)^ := M;
end;

procedure quant_dc_2x2(a: int16_p; const qp: uint8_t);
var
  i: int32_t;
  f: int32_t;
  qbits: uint8_t;
  MF: int16_t;
begin
  // multiply factor
  MF := mult_factor[table_qp_mod6[qp], 0];
  // multiply shift
  qbits := 16 + table_qp_div6[qp];
  f := 1 shl (qbits - 1);

  for i := 0 to 3 do
    if a[i] > 0 then
        a[i] := (a[i] * MF + f) shr qbits
    else
        a[i] := -((f - a[i] * MF) shr qbits);
end;

procedure iquant_dc_2x2(a: int16_p; const qp: uint8_t);
var
  i: int32_t;
  Shift: int32_t;
  MF: int16_t;
begin
  Shift := table_qp_div6[qp] - 1;
  MF := resc_factor[table_qp_mod6[qp], 0];
  if qp >= 6 then
    begin
      for i := 0 to 3 do
          a[i] := a[i] * MF shl Shift;
    end
  else
    for i := 0 to 3 do
        a[i] := SAR16(a[i] * MF, 1);
end;

procedure transqt_dc_2x2(Block: int16_p; const qp: uint8_t);
begin
  trans_dc_2x2(Block);
  quant_dc_2x2(Block, qp);
end;

procedure itransqt_dc_2x2(Block: int16_p; const qp: uint8_t);
begin
  trans_dc_2x2(Block);
  iquant_dc_2x2(Block, qp);
end;

procedure itrans_dc(Block: int16_p);
var
  DC: int16_t;
  i: int32_t;
begin
  DC := SAR16(Block[0] + 32, 6);
  for i := 0 to 15 do
      Block[i] := DC;
end;

(* ******************************************************************************
  luma DC 4x4
*)
procedure core_4x4_dc(Block: int16_p);
var
  M: matrix_t;
  E, f, g, h: array [0 .. 3] of int16_t;
  i: int32_t;
begin
  CopyPtr(Block, @M, 16 * 2);

  for i := 0 to 3 do
    begin
      E[i] := M[0][i] + M[3][i]; // a + d
      f[i] := M[0][i] - M[3][i]; // a - d
      g[i] := M[1][i] + M[2][i]; // b + c
      h[i] := M[1][i] - M[2][i]; // b - c
    end;

  for i := 0 to 3 do
    begin
      M[0][i] := E[i] + g[i]; // a + b + c + d
      M[1][i] := f[i] + h[i]; // a + b - c - d
      M[2][i] := E[i] - g[i]; // a - b - c + d
      M[3][i] := f[i] - h[i]; // a - b + c - d
    end;

  for i := 0 to 3 do
    begin
      E[i] := M[i][0] + M[i][3];
      f[i] := M[i][0] - M[i][3];
      g[i] := M[i][1] + M[i][2];
      h[i] := M[i][1] - M[i][2];
    end;

  for i := 0 to 3 do
    begin
      M[i][0] := E[i] + g[i];
      M[i][1] := f[i] + h[i];
      M[i][2] := E[i] - g[i];
      M[i][3] := f[i] - h[i];
    end;

  CopyPtr(@M, Block, 16 * 2);
end;

procedure quant_dc_4x4(a: int16_p; const qp: uint8_t);
var
  i: int32_t;
  f: int32_t;
  qbits: uint8_t;
  MF: int16_p;
begin
  // scale by 2
  for i := 0 to 15 do
    if a[i] > 0 then
        a[i] := (a[i] + 1) div 2
    else
        a[i] := (a[i] - 1) div 2;

  // multiply factor
  MF := @mult_factor[table_qp_mod6[qp]];
  // multiply shift
  qbits := 16 + table_qp_div6[qp];
  f := 1 shl (qbits - 1);

  for i := 0 to 15 do
    if a[i] > 0 then
        a[i] := (a[i] * MF[0] + f) shr qbits
    else
        a[i] := -((f - a[i] * MF[0]) shr qbits);
end;

procedure iquant_dc_4x4(a: int16_p; const qp: uint8_t);
var
  i: int32_t;
  f, Shift: int32_t;
  MF: int32_t;
begin
  MF := resc_factor[table_qp_mod6[qp], 0];

  if qp >= 12 then
    begin
      Shift := table_qp_div6[qp] - 2;
      for i := 0 to 15 do
          a[i] := a[i] * MF shl Shift;
    end
  else
    begin
      Shift := 2 - table_qp_div6[qp];
      f := 1 shl (1 - table_qp_div6[qp]);
      for i := 0 to 15 do
          a[i] := SAR16(a[i] * MF + f, Shift);
    end;
end;

procedure transqt_dc_4x4(Block: int16_p; const qp: uint8_t);
begin
  core_4x4_dc(Block);
  quant_dc_4x4(Block, qp);
end;

procedure itransqt_dc_4x4(Block: int16_p; const qp: uint8_t);
begin
  core_4x4_dc(Block);
  iquant_dc_4x4(Block, qp);
end;

initialization

init_tables;

end.  
 
 
