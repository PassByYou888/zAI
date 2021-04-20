{ ****************************************************************************** }
{ * h264Macroblock.pas        by qq600585                                      * }
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

unit h264Macroblock;

{$INCLUDE zDefine.inc}
{$POINTERMATH ON}

interface

uses
  h264Types, h264Common, h264Util, h264Pixel, h264Intra_pred, h264Transquant, h264VLC, h264tables, CoreClasses;

procedure mb_alloc(var mb: TMacroblock);
procedure mb_free(var mb: TMacroblock);
procedure mb_init_row_ptrs(var mb: TMacroblock; const frame: TFrame; const y: int32_t);
procedure mb_init(var mb: TMacroblock; var frame: TFrame; const adaptive_quant: Boolean);

procedure encode_mb_intra_i4(var mb: TMacroblock; var frame: TFrame; const intrapred: TIntraPredictor);

procedure encode_mb_intra_i16(var mb: TMacroblock);
procedure decode_mb_intra_i16(var mb: TMacroblock; const intrapred: TIntraPredictor);

procedure encode_mb_inter(var mb: TMacroblock);
procedure decode_mb_inter(var mb: TMacroblock);

procedure decode_mb_inter_pskip(var mb: TMacroblock);
procedure decode_mb_pcm(var mb: TMacroblock);

procedure encode_mb_chroma(var mb: TMacroblock; const intrapred: TIntraPredictor; const intra: Boolean);
procedure decode_mb_chroma(var mb: TMacroblock; const intra: Boolean);

implementation

procedure mb_alloc(var mb: TMacroblock);
var
  i: int32_t;
begin
  // memory layout: pixels | pred | mcomp | pixels_dec | pixels uv | pred | mcomp uv | pixels_dec uv
  mb.pixels := fev_malloc(256 * 4 { luma } + 64 * 2 * 4 { chroma } );
  mb.pred := mb.pixels + 256;
  mb.mcomp := mb.pred + 256;
  mb.pixels_dec := mb.mcomp + 256;

  mb.pixels_c[0] := mb.pixels + 256 * 4;
  mb.pixels_c[1] := mb.pixels_c[0] + 8;
  mb.pred_c[0] := mb.pixels_c[0] + 128;
  mb.pred_c[1] := mb.pred_c[0] + 8;
  mb.mcomp_c[0] := mb.pred_c[0] + 128;
  mb.mcomp_c[1] := mb.mcomp_c[0] + 8;
  mb.pixels_dec_c[0] := mb.mcomp_c[0] + 128;
  mb.pixels_dec_c[1] := mb.pixels_dec_c[0] + 8;

  mb.dct[0] := fev_malloc(2 * 16 * 25);
  for i := 1 to 24 do
      mb.dct[i] := mb.dct[i - 1] + 16;
end;

procedure mb_free(var mb: TMacroblock);
begin
  fev_free(mb.pixels);
  fev_free(mb.dct[0]);
end;

procedure mb_init_row_ptrs(var mb: TMacroblock; const frame: TFrame; const y: int32_t);
begin
  mb.pfenc := frame.plane[0] + y * 16 * frame.stride;
  mb.pfenc_c[0] := frame.plane[1] + y * 8 * frame.stride_c;
  mb.pfenc_c[1] := frame.plane[2] + y * 8 * frame.stride_c;

  mb.pfdec := frame.plane_dec[0] + y * 16 * frame.stride;
  mb.pfdec_c[0] := frame.plane_dec[1] + y * 8 * frame.stride_c;
  mb.pfdec_c[1] := frame.plane_dec[2] + y * 8 * frame.stride_c;
end;

{ fill I16x16 prediction pixel cache
  0,17 - top left pixel
  1..16 - pixels from top
  18..33 - pixels from left
}
procedure fill_intra_pred_cache(var mb: TMacroblock; var frame: TFrame);
var
  i: int32_t;
  Src, Dst: uint8_p;
begin
  Dst := @mb.intra_pixel_cache;
  // top - use pixels from decoded frame
  Src := mb.pfdec - frame.stride - 1;
  CopyPtr(Src, Dst, 17);
  // top left
  Dst[17] := Dst[0];
  // left - use rightmost pixel row from previously decoded mb
  Src := mb.pixels_dec + 15;
  for i := 0 to 15 do
      Dst[i + 18] := Src[i * 16];
end;

(* ******************************************************************************
  initialize mb structure:
  -intra prediction
  -non-zero count
  -qp
  -mvd, skip mv
*)
procedure mb_init(var mb: TMacroblock; var frame: TFrame; const adaptive_quant: Boolean);
var
  mbb, mba: PMacroblock;
  i: int32_t;
begin
  FillPtrByte(@mb.i4_pred_mode, 24, INTRA_PRED_NA);
  mb.chroma_pred_mode := INTRA_PRED_CHROMA_DC;

  FillPtrByte(@mb.nz_coef_cnt, 24, NZ_COEF_CNT_NA);
  for i := 0 to 7 do
      mb.nz_coef_cnt_chroma_ac[0, i] := NZ_COEF_CNT_NA;
  for i := 0 to 7 do
      mb.nz_coef_cnt_chroma_ac[1, i] := NZ_COEF_CNT_NA;

  mb.mba := nil;
  mb.mbb := nil;

  // top mb
  if mb.y > 0 then
    begin
      mbb := @frame.mbs[(mb.y - 1) * frame.mbw + mb.x];
      mb.mbb := mbb;

      if mbb^.mbtype = MB_I_4x4 then
        begin
          mb.i4_pred_mode[16] := mbb^.i4_pred_mode[10];
          mb.i4_pred_mode[17] := mbb^.i4_pred_mode[11];
          mb.i4_pred_mode[18] := mbb^.i4_pred_mode[14];
          mb.i4_pred_mode[19] := mbb^.i4_pred_mode[15];
        end
      else
        begin
          mb.i4_pred_mode[16] := INTRA_PRED_DC;
          mb.i4_pred_mode[17] := INTRA_PRED_DC;
          mb.i4_pred_mode[18] := INTRA_PRED_DC;
          mb.i4_pred_mode[19] := INTRA_PRED_DC;
        end;

      mb.nz_coef_cnt[16] := mbb^.nz_coef_cnt[10];
      mb.nz_coef_cnt[17] := mbb^.nz_coef_cnt[11];
      mb.nz_coef_cnt[18] := mbb^.nz_coef_cnt[14];
      mb.nz_coef_cnt[19] := mbb^.nz_coef_cnt[15];

      for i := 0 to 1 do
        begin
          mb.nz_coef_cnt_chroma_ac[i, 4] := mbb^.nz_coef_cnt_chroma_ac[i, 2];
          mb.nz_coef_cnt_chroma_ac[i, 5] := mbb^.nz_coef_cnt_chroma_ac[i, 3];
        end;
    end;

  // left mb
  if mb.x > 0 then
    begin
      mba := @frame.mbs[mb.y * frame.mbw + mb.x - 1];
      mb.mba := mba;

      if mba^.mbtype = MB_I_4x4 then
        begin
          mb.i4_pred_mode[20] := mba^.i4_pred_mode[5];
          mb.i4_pred_mode[21] := mba^.i4_pred_mode[7];
          mb.i4_pred_mode[22] := mba^.i4_pred_mode[13];
          mb.i4_pred_mode[23] := mba^.i4_pred_mode[15];
        end
      else
        begin
          mb.i4_pred_mode[20] := INTRA_PRED_DC;
          mb.i4_pred_mode[21] := INTRA_PRED_DC;
          mb.i4_pred_mode[22] := INTRA_PRED_DC;
          mb.i4_pred_mode[23] := INTRA_PRED_DC;
        end;

      mb.nz_coef_cnt[20] := mba^.nz_coef_cnt[5];
      mb.nz_coef_cnt[21] := mba^.nz_coef_cnt[7];
      mb.nz_coef_cnt[22] := mba^.nz_coef_cnt[13];
      mb.nz_coef_cnt[23] := mba^.nz_coef_cnt[15];

      for i := 0 to 1 do
        begin
          mb.nz_coef_cnt_chroma_ac[i, 6] := mba^.nz_coef_cnt_chroma_ac[i, 1];
          mb.nz_coef_cnt_chroma_ac[i, 7] := mba^.nz_coef_cnt_chroma_ac[i, 3];
        end;
    end;

  // qp
  mb.qp := frame.qp;
  if adaptive_quant then
      mb.qp := frame.aq_table[mb.y * frame.mbw + mb.x];
  if mb.qp < 30 then
      mb.qpc := mb.qp
  else
      mb.qpc := tab_qp_chroma[mb.qp];
  inc(mb.qpc, mb.chroma_qp_offset);

  // intra cache
  fill_intra_pred_cache(mb, frame);
end;

(* ******************************************************************************
  intra coding
*)
const
  SAD_DECIMATE_TRESH: array [0 .. 51] of uint16_t = (
    3, 3, 3, 3, 3, 3, 4, 4, 5, 5,
    6, 7, 8, 9, 10, 11, 13, 14, 16, 18,
    20, 22, 26, 28, 32, 36, 40, 44, 52, 56,
    64, 72, 80, 88, 104, 112, 128, 144, 160, 176,
    208, 224, 256, 288, 320, 352, 416, 448, 512, 576,
    640, 704
    );

procedure block_use_zero(var b: TBlock);
begin
  b.t0 := 0;
  b.t1 := 0;
  b.nlevel := 0;
end;

procedure encode_mb_intra_i4
  (var mb: TMacroblock; var frame: TFrame; const intrapred: TIntraPredictor);
var
  i: int32_t;
  Block: int16_p;
  cbp: array [0 .. 3] of uint8_t;
  sad, sad_tresh: int32_t;

begin
  for i := 0 to 3 do
      cbp[i] := 0;
  sad_tresh := SAD_DECIMATE_TRESH[mb.qp];

  for i := 0 to 15 do
    begin
      Block := mb.dct[i];

      mb.i4_pred_mode[i] := intrapred.Analyse_4x4(mb.pfdec + frame.blk_offset[i], mb.x, mb.y, i);
      sad := DSP.sad_4x4(mb.pixels + block_offset4[i], mb.pred + block_offset4[i], 16);
      if sad >= sad_tresh then
        begin
          DSP.pixel_sub_4x4(mb.pixels + block_offset4[i], mb.pred + block_offset4[i], Block);
          transqt(Block, mb.qp, True);
          cavlc_analyse_block(mb.Block[i], Block, 16);
        end
      else
          block_use_zero(mb.Block[i]);

      mb.nz_coef_cnt[i] := mb.Block[i].nlevel;
      inc(cbp[i div 4], mb.nz_coef_cnt[i]);

      // decode block
      if mb.nz_coef_cnt[i] > 0 then
        begin
          itransqt(Block, mb.qp);
          DSP.pixel_add_4x4(mb.pixels_dec + block_offset4[i], mb.pred + block_offset4[i], Block);
        end
      else
          pixel_load_4x4(mb.pixels_dec + block_offset4[i], mb.pred + block_offset4[i], 16);

      pixel_save_4x4(mb.pixels_dec + block_offset4[i],
        mb.pfdec + frame.blk_offset[i], frame.stride);
    end;

  mb.cbp := 0;
  for i := 0 to 3 do
    if cbp[i] > 0 then
        mb.cbp := mb.cbp or (1 shl i);
end;

procedure encode_mb_intra_i16(var mb: TMacroblock);
var
  i: int32_t;
  Block: int16_p;
  cbp: uint8_t;

begin
  cbp := 0;

  for i := 0 to 15 do
    begin
      Block := mb.dct[i];

      DSP.pixel_sub_4x4(mb.pixels + block_offset4[i], mb.pred + block_offset4[i], Block);
      transqt(Block, mb.qp, True, 1);

      mb.dct[24][block_dc_order[i]] := Block[0];

      cavlc_analyse_block(mb.Block[i], Block, 15);
      mb.nz_coef_cnt[i] := mb.Block[i].nlevel;
      inc(cbp, mb.nz_coef_cnt[i]);
    end;

  // dc transform
  transqt_dc_4x4(mb.dct[24], mb.qp);
  cavlc_analyse_block(mb.Block[24], mb.dct[24], 16);
  mb.nz_coef_cnt_dc := mb.Block[24].nlevel;

  // cbp: only 0 or 15
  if cbp = 0 then
      mb.cbp := 0
  else
      mb.cbp := $F;
end;

procedure decode_mb_intra_i16(var mb: TMacroblock; const intrapred: TIntraPredictor);
var
  i: int32_t;
  Block: int16_p;

begin
  intrapred.Predict_16x16(mb.i16_pred_mode, mb.x, mb.y);

  itransqt_dc_4x4(mb.dct[24], mb.qp);

  for i := 0 to 15 do
    begin
      Block := mb.dct[i];
      Block[0] := mb.dct[24][block_dc_order[i]];

      if mb.nz_coef_cnt[i] > 0 then
          itransqt(Block, mb.qp, 1)
      else
          itrans_dc(Block);

      DSP.pixel_add_4x4(mb.pixels_dec + block_offset4[i], mb.pred + block_offset4[i], Block);
    end;
end;

(* ******************************************************************************
  inter coding
*)
procedure encode_mb_inter(var mb: TMacroblock);
var
  i: int32_t;
  Block: int16_p;
  cbp: array [0 .. 3] of uint8_t;
  sad, sad_tresh: int32_t;

begin
  for i := 0 to 3 do
      cbp[i] := 0;
  sad_tresh := SAD_DECIMATE_TRESH[mb.qp];

  for i := 0 to 15 do
    begin
      Block := mb.dct[i];

      sad := DSP.sad_4x4(mb.pixels + block_offset4[i], mb.mcomp + block_offset4[i], 16);
      if sad >= sad_tresh then
        begin
          DSP.pixel_sub_4x4(mb.pixels + block_offset4[i], mb.mcomp + block_offset4[i], Block);
          transqt(Block, mb.qp, False);
          cavlc_analyse_block(mb.Block[i], Block, 16);
        end
      else
          block_use_zero(mb.Block[i]);

      mb.nz_coef_cnt[i] := mb.Block[i].nlevel;
      inc(cbp[i div 4], mb.nz_coef_cnt[i]);
    end;

  mb.cbp := 0;
  for i := 0 to 3 do
    if cbp[i] > 0 then
        mb.cbp := mb.cbp or (1 shl i);
end;

procedure decode_mb_inter(var mb: TMacroblock);
var
  i: int32_t;
  Block: int16_p;
begin
  for i := 0 to 15 do
    begin
      Block := mb.dct[i];

      if mb.nz_coef_cnt[i] > 0 then
        begin
          itransqt(Block, mb.qp);
          DSP.pixel_add_4x4(mb.pixels_dec + block_offset4[i], mb.mcomp + block_offset4[i], Block);
        end
      else
          pixel_save_4x4(mb.mcomp + block_offset4[i], mb.pixels_dec + block_offset4[i], 16);
    end;
end;

procedure decode_mb_inter_pskip(var mb: TMacroblock);
begin
  CopyPtr(mb.mcomp, mb.pixels_dec, 256);
  FillPtrByte(@mb.nz_coef_cnt, 16, 0);
  mb.cbp := 0;
end;

(* ******************************************************************************
  chroma coding
*)
procedure encode_mb_chroma(var mb: TMacroblock; const intrapred: TIntraPredictor; const intra: Boolean);
var
  i, j, n: int32_t;
  Block: int16_p;
  pred: uint8_p;
  cbp_ac: uint8_t;
  sad, sad_tresh: int32_t;

begin
  cbp_ac := 0;
  for i := 0 to 3 do
    begin
      mb.chroma_dc[0, i] := 0;
      mb.chroma_dc[1, i] := 0;
    end;
  sad_tresh := SAD_DECIMATE_TRESH[mb.qpc];

  if intra then
      intrapred.Analyse_8x8_cr(mb.pfdec_c[0], mb.pfdec_c[1], mb.x, mb.y, mb.chroma_pred_mode);

  for j := 0 to 1 do
    begin
      if intra then
          pred := mb.pred_c[j]
      else
          pred := mb.mcomp_c[j];

      for i := 0 to 3 do
        begin
          n := 16 + i + j * 4;
          Block := mb.dct[n];

          sad := DSP.sad_4x4(mb.pixels_c[j] + block_offset_chroma[i], pred + block_offset_chroma[i], 16);
          if sad >= sad_tresh then
            begin
              DSP.pixel_sub_4x4(mb.pixels_c[j] + block_offset_chroma[i], pred + block_offset_chroma[i], Block);
              transqt(Block, mb.qpc, False, 1);
              mb.chroma_dc[j, i] := Block[0];
              cavlc_analyse_block(mb.Block[n], Block, 15);
            end
          else
            begin
              block_use_zero(mb.Block[n]);
              mb.chroma_dc[j, i] := 0;
            end;

          mb.nz_coef_cnt_chroma_ac[j, i] := mb.Block[n].nlevel;
          if mb.nz_coef_cnt_chroma_ac[j, i] > 0 then
              inc(cbp_ac);
        end;
    end;

  // dc transform
  for j := 0 to 1 do
    begin
      transqt_dc_2x2(@mb.chroma_dc[j], mb.qpc);
      cavlc_analyse_block(mb.Block[25 + j], @mb.chroma_dc[j], 4);
    end;

  // cbp
  if cbp_ac > 0 then
      mb.cbp := mb.cbp or (1 shl 5)
  else
    if (mb.Block[25].nlevel + mb.Block[26].nlevel > 0) then
      mb.cbp := mb.cbp or (1 shl 4);
end;

procedure decode_mb_chroma(var mb: TMacroblock; const intra: Boolean);
var
  i, j: int32_t;
  Block: int16_p;
  pred: uint8_p;
begin
  if mb.cbp shr 4 = 0 then
    begin
      // shortcut for no chroma residual case
      if intra then
          CopyPtr(mb.pred_c[0], mb.pixels_dec_c[0], 128)
      else
        begin
          CopyPtr(mb.mcomp_c[0], mb.pixels_dec_c[0], 128);

          if (mb.mbtype = MB_P_SKIP) then
              FillPtrByte(@mb.nz_coef_cnt_chroma_ac, 16, 0);
        end;

    end
  else
    begin

      for j := 0 to 1 do
          itransqt_dc_2x2(@mb.chroma_dc[j], mb.qpc);

      for j := 0 to 1 do
        begin
          if intra then
              pred := mb.pred_c[j]
          else
              pred := mb.mcomp_c[j];

          for i := 0 to 3 do
            begin
              Block := mb.dct[16 + i + j * 4];
              Block[0] := mb.chroma_dc[j, i];

              if mb.nz_coef_cnt_chroma_ac[j, i] > 0 then
                  itransqt(Block, mb.qpc, 1)
              else
                  itrans_dc(Block);

              DSP.pixel_add_4x4(mb.pixels_dec_c[j] + block_offset_chroma[i],
                pred + block_offset_chroma[i], Block);
            end;
        end;

    end;
end;

// I_PCM
procedure decode_mb_pcm(var mb: TMacroblock);
begin
  CopyPtr(mb.pixels, mb.pixels_dec, 256);
  CopyPtr(mb.pixels_c[0], mb.pixels_dec_c[0], 128);
  FillPtrByte(@mb.nz_coef_cnt, 16, 0); // todo unnecessary?
  mb.cbp := 0;
end;

end.  
 
 
