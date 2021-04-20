{ ****************************************************************************** }
{ * h264stream.pas        by qq600585                                          * }
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

unit h264stream;

{$INCLUDE zDefine.inc}

interface

uses
  h264Util, h264Types, h264Common, h264VLC, h264BitStream, h264tables, PascalStrings;

type
  // SPS
  sps_t = record
    width, height: int32_t;
    mb_width, mb_height: int32_t;
    pic_order_cnt_type: uint8_t;
    num_ref_frames: uint8_t;
    log2_max_frame_num_minus4: uint8_t;
    log2_max_pic_order_cnt_lsb_minus4: uint8_t;
  end;

  // PPS
  pps_t = record
    deblocking_filter_control_present_flag: uint8_t;
    qp: uint8_t;
    chroma_qp_offset: int8_t;
  end;

  // slice
  slice_header_t = record
    type_: uint8_t;
    is_idr: Boolean;
    idr_pic_id: uint16_t;
    frame_num: int32_t;
    qp: int32_t;
    slice_qp_delta: int32_t;
    num_ref_frames: uint8_t;
  end;

  TH264InterPredCostEvaluator = class;

  { TH264Stream }

  TH264Stream = class
  private
    sps: sps_t;
    pps: pps_t;
    write_sei: Boolean;
    write_vui: Boolean;
    sei_string: TPascalString;

    mb_skip_count: int32_t;
    bs: TBitStreamWriter;
    slice: slice_header_t;
    last_mb_qp: uint8_t;

    interPredCostEval: TH264InterPredCostEvaluator;
    cabac: Boolean;

    function GetNoPSkip: Boolean;
    procedure SetChromaQPOffset(const AValue: uint8_t);
    procedure SetKeyInterval(const AValue: uint16_t);
    procedure SetNumRefFrames(const AValue: uint8_t);
    procedure SetQP(const AValue: uint8_t);
    function GetSEI: TPascalString;
    procedure SetSEI(const AValue: TPascalString);
    procedure WriteSliceHeader;
    procedure WriteParamSetsToNAL(var nalstream: TBitStreamWriter);

    procedure write_mb_pred_intra(const mb: TMacroblock);
    procedure write_mb_pred_inter(const mb: TMacroblock);
    procedure write_mb_residual(var mb: TMacroblock);
    procedure write_mb_i_pcm(var mb: TMacroblock);
    procedure write_mb_i_4x4(var mb: TMacroblock);
    procedure write_mb_i_16x16(var mb: TMacroblock);
    procedure write_mb_p_16x16(var mb: TMacroblock);
    procedure write_mb_p_skip;
    function mb_intrapred_bits(const mb: TMacroblock): int32_t;
    function mb_residual_bits(const mb: TMacroblock): int32_t;
    function mb_i_4x4_bits(const mb: TMacroblock): int32_t;
    function mb_i_16x16_bits(const mb: TMacroblock): int32_t;
    function mb_p_16x16_bits(const mb: TMacroblock): int32_t;
    function mb_p_skip_bits: int32_t;
    function mb_interpred_bits(const mb: TMacroblock): int32_t;

  public
    property NumRefFrames: uint8_t read slice.num_ref_frames write SetNumRefFrames;
    property qp: uint8_t write SetQP;
    property ChromaQPOffset: uint8_t write SetChromaQPOffset;
    property KeyInterval: uint16_t write SetKeyInterval;
    property SEIString: TPascalString read GetSEI write SetSEI;
    property NoPSkipAllowed: Boolean read GetNoPSkip;

    constructor Create(w, h, mbw, mbh: int32_t);
    destructor Destroy; override;
    procedure DisableLoopFilter;
    procedure InitSlice(slicetype, slice_qp, ref_frame_count: int32_t; bs_buffer: uint8_p);
    procedure AbortSlice;
    procedure GetSliceBitstream(var buffer: uint8_p; out Size: uint32_t);
    procedure WriteMB(var mb: TMacroblock);
    function GetBitCost(const mb: TMacroblock): int32_t;
    function GetInterPredCostEvaluator: IInterPredCostEvaluator;
  end;

  TH264InterPredCostEvaluator = class(IInterPredCostEvaluator)
  private
    _h264stream: TH264Stream;
    _lambda: int32_t;
    _mvp: TMotionvec;
    _ref_idx: int32_t;
    _ref_frame_bits: int32_t;
  public
    constructor Create(const h264stream: TH264Stream);
    procedure SetQP(qp: int32_t); override;
    procedure SetMVPredAndRefIdx(const mvp: TMotionvec; const idx: int32_t); override;
    function bitcost(const mv: TMotionvec): int32_t; override;
  end;

implementation

uses DoStatusIO;

const
  // type codes
  NAL_NOIDR = 1; // Coded slice of a non-IDR picture
  NAL_IDR   = 5; // non-partitioned
  NAL_SEI   = 6;
  NAL_SPS   = 7;
  NAL_PPS   = 8;

  // Level limits
  LEVEL_DPB: array [0 .. 14, 0 .. 1] of int32_t = (
    (10, 148),
    (11, 337),
    (12, 891),
    (13, 891),
    (20, 891),
    (21, 1782),
    (22, 3037),
    (30, 3037),
    (31, 6750),
    (32, 7680),
    (40, 12288),
    (41, 12288),
    (42, 12288),
    (50, 41400),
    (51, 69120)
    );

function slice2naltype(i: int32_t): int32_t;
begin
  case i of
    SLICE_I:
      Result := NAL_IDR;
    else
      Result := NAL_NOIDR;
  end;
end;

function get_level(const w, h, refcount: int32_t): uint8_t;
var
  dpb, i: int32_t;
begin
  dpb := w * h * 3 div 2 * (refcount + 1) div 1024;
  if dpb > 69120 then
    begin // oops, dpb too big
      Result := 0;
    end
  else
    begin
      i := 0;
      while LEVEL_DPB[i, 1] < dpb do
          inc(i);
      Result := uint8_t(LEVEL_DPB[i, 0]);
    end;
end;

{
  3.104 raw uint8_t sequence payload (RBSP): A syntax structure containing an int32_t number of bytes that is
  encapsulated in a NAL unit. An RBSP is either empty or has the form of a TPascalString of data bits containing syntax
  elements followed by an RBSP stop bit and followed by zero or more subsequent bits equal to 0.

  3.105 raw uint8_t sequence payload (RBSP) stop bit: A bit equal to 1 present within a raw uint8_t sequence payload
  (RBSP) after a TPascalString of data bits. The location of the end of the TPascalString of data bits within an RBSP can be
  identified by searching from the end of the RBSP for the RBSP stop bit, which is the last non-zero bit in the
  RBSP.


  NAL + annex B

  3.130 start code prefix: A unique sequence of three bytes equal to 0x000001 embedded in the uint8_t stream as a prefix
  to each NAL unit. The location of a start code prefix can be used by a decoder to identify the beginning of a
  new NAL unit and the end of a previous NAL unit. Emulation of start code prefixes is prevented within NAL
  units by the inclusion of emulation prevention bytes.

  7.4.1
  emulation_prevention_three_byte is a uint8_t equal to 0x03. When an emulation_prevention_three_byte is present in the
  NAL unit, it shall be discarded by the decoding process.
  The last uint8_t of the NAL unit shall not be equal to 0x00.
  Within the NAL unit, the following three-uint8_t sequences shall not occur at any uint8_t-aligned position:
  0x000000
  0x000001
  0x000002
  Within the NAL unit, any four-uint8_t sequence that starts with 0x000003 other than the following sequences shall not
  occur at any uint8_t-aligned position:
  0x00000300
  0x00000301
  0x00000302
  0x00000303
}
// NAL encapsulate RBSP (raw uint8_t seq.payload)
procedure NAL_encapsulate(var rbsp: TBitStreamWriter; var nalstream: TBitStreamWriter; const naltype: int32_t);
var
  nal_ref_idc: int32_t;
  i, Len: int32_t;
  a: uint8_p;
begin
  nal_ref_idc := 3;
  // rbsp_trailing_bits
  rbsp.write(1);
  rbsp.ByteAlign;
  rbsp.Close;
  a := rbsp.DataStart;
  Len := rbsp.ByteSize;
  nal_ref_idc := 3;
  if naltype = NAL_SEI then
      nal_ref_idc := 0;

  // annex B:  0x00000001
  nalstream.write(1, 32);
  // nal: forbidden_zero_bit | nal_ref_idc | nal_unit_type
  nalstream.write((nal_ref_idc shl 5) or naltype, 8);
  // emulation prevention
  i := 0;
  while i < Len do
    begin
      // cycle to catch repeated occurences
      while (i + 2 < Len) and (a[0] = 0) and (a[1] = 0) and (a[2] in [0, 1, 2, 3]) do
        begin
          nalstream.write(3, 24); // 0x000003
          inc(a, 2);
          inc(i, 2);
        end;
      nalstream.write(a^, 8);
      inc(a);
      inc(i);
    end;
end;

{
  write SPS/PPS to NAL unit

  bits(SODB) -> RBSP

  3.131 TPascalString of data bits (SODB): A sequence of some number of bits representing syntax elements present within a
  raw uint8_t sequence payload prior to the raw uint8_t sequence payload stop bit. Within an SODB, the left-most bit
  is considered to be the first and most significant bit, and the right-most bit is considered to be the last and least
  significant bit.


  7.3.2.1 Sequence parameter set RBSP syntax

  profile: baseline
}

procedure TH264Stream.WriteParamSetsToNAL(var nalstream: TBitStreamWriter);
var
  b: TBitStreamWriter;
  rbsp: array [0 .. 255] of uint8_t;
  i: int32_t;
  sei_uuid: TPascalString;
  sei_text: TPascalString;
  level: uint8_t;
begin
  sei_uuid.Text := '2011012520091007';
  level := get_level(sps.mb_width * 16, sps.mb_height * 16, sps.num_ref_frames);

  // SPS
  b := TBitStreamWriter.Create(@rbsp);

  b.write(66, 8);    // profile_idc u(8) (annex A)
  b.write(1);        // constraint_set0_flag u(1)
  b.write(0);        // constraint_set1_flag u(1)
  b.write(0);        // constraint_set2_flag u(1)
  b.write(0, 5);     // reserved_zero_5bits /* equal to 0 */ 0 u(5)
  b.write(level, 8); // level_idc 0 u(8)

  write_ue_code(b, 0); // seq_parameter_set_id 0 ue(v)
  write_ue_code(b, sps.log2_max_frame_num_minus4);
  // log2_max_frame_num_minus4 0 ue(v)
  write_ue_code(b, sps.pic_order_cnt_type);
  // pic_order_cnt_type ue(v)
  if sps.pic_order_cnt_type = 0 then
      write_ue_code(b, sps.log2_max_pic_order_cnt_lsb_minus4);
  // log2_max_pic_order_cnt_lsb_minus4
  write_ue_code(b, sps.num_ref_frames);
  // num_ref_frames  ue(v)
  b.write(0);                          // gaps_in_frame_num_value_allowed_flag 0 u(1)
  write_ue_code(b, sps.mb_width - 1);  // pic_width_in_mbs_minus1 0 ue(v)
  write_ue_code(b, sps.mb_height - 1); // pic_height_in_map_units_minus1 0 ue(v)
  b.write(1);                          // frame_mbs_only_flag         u(1)
  b.write(0);                          // direct_8x8_inference_flag   u(1)

  // cropping
  if ((sps.width or sps.height) and $F) = 0 then
      b.write(0) // frame_cropping_flag         u(1)
  else
    begin
      b.write(1);          // offsets:
      write_ue_code(b, 0); // left, right
      write_ue_code(b, (sps.mb_width * 16 - sps.width) div 2);
      write_ue_code(b, 0); // top, bottom
      write_ue_code(b, (sps.mb_height * 16 - sps.height) div 2);
    end;

  // VUI
  if write_vui then
    begin
      b.write(1); // vui_parameters_present_flag u(1)
      b.write(0); // aspect_ratio_info_present_flag
      b.write(0); // overscan_info_present_flag
      b.write(0); // video_signal_type_present_flag
      b.write(0); // chroma_loc_info_present_flag
      b.write(1); // timing_info_present_flag
      // if( timing_info_present_flag )
      b.write(1, 32);  // num_units_in_tick
      b.write(50, 32); // time_scale
      b.write(1);      // fixed_frame_rate_flag
      b.write(0);      // nal_hrd_parameters_present_flag
      b.write(0);      // vcl_hrd_parameters_present_flag
      b.write(0);      // pic_struct_present_flag
      b.write(0);      // bitstream_restriction_flag
    end
  else
      b.write(0);

  NAL_encapsulate(b, nalstream, NAL_SPS);
  b.Free;

  // PPS
  b := TBitStreamWriter.Create(@rbsp);

  write_ue_code(b, 0); // pic_parameter_set_id   ue(v)
  write_ue_code(b, 0); // seq_parameter_set_id   ue(v)
  if cabac then
      b.write(1)
  else
      b.write(0);
  // entropy_coding_mode_flag  u(1)
  b.write(0);          // pic_order_present_flag    u(1)
  write_ue_code(b, 0); // num_slice_groups_minus1   ue(v)

  write_ue_code(b, sps.num_ref_frames - 1); // num_ref_idx_l0_active_minus1 ue(v)
  write_ue_code(b, 0);                      // num_ref_idx_l1_active_minus1 ue(v)
  b.write(0);                               // weighted_pred_flag  u(1)
  b.write(0, 2);                            // weighted_bipred_idc u(2)

  write_se_code(b, pps.qp - 26);
  // pic_init_qp_minus26 /* relative to 26 */ 1 se(v)
  write_se_code(b, 0); // pic_init_qs_minus26 /* relative to 26 */ 1 se(v)
  write_se_code(b, pps.chroma_qp_offset);
  // chroma_qp_index_offset [-12.. 12] se(v)

  b.write(pps.deblocking_filter_control_present_flag); // deblocking_filter_control_present_flag 1 u(1)
  b.write(0);                                          // constrained_intra_pred_flag    u(1)
  b.write(0);                                          // redundant_pic_cnt_present_flag u(1)

  // T-REC-H.264-200503
  {
    b.Write(0);                 //8x8 transform flag
    b.Write(0);                 //scaling matrix
    write_se_code(b, -3);       //second chroma qp offset
  }

  NAL_encapsulate(b, nalstream, NAL_PPS);
  b.Free;

  // sei; payload_type = 5 (user_data_unregistered)
  if write_sei then
    begin
      sei_text := 'fevh264 ' + sei_string;

      b := TBitStreamWriter.Create(@rbsp);
      b.write(5, 8); // last_payload_type_byte

      i := sei_uuid.Len + sei_text.Len;
      while i > 255 do
        begin
          b.write(255, 8); // ff_byte
          dec(i, 255);
        end;
      b.write(i, 8); // last_payload_size_byte
      for i := 1 to sei_uuid.Len do
          b.write(uint8_t(sei_uuid[i]), 8);
      for i := 1 to sei_text.Len do
          b.write(uint8_t(sei_text[i]), 8);

      NAL_encapsulate(b, nalstream, NAL_SEI);
      b.Free;
    end;

  // write aux. info only once
  write_vui := False;
  write_sei := False;
end;

(* ******************************************************************************
  7.3.2.8 Slice layer without partitioning RBSP syntax
  slice_layer_without_partitioning_rbsp( ) {
  slice_header( )
  slice_data( ) /* all categories of slice_data( ) syntax */
  rbsp_slice_trailing_bits( )
  }
  ****************************************************************************** *)

{
  slice header

  slicetype
  0,1,2 + 5,6,7 = p,b,i
  if slicetype > 4: all slicetypes in current frame will be equal

  frame_num
  is used as an identifier for pictures and shall be represented by log2_max_frame_num_minus4 + 4 bits in the bitstream.
  frame_num = 0 for IDR slices
}
procedure TH264Stream.WriteSliceHeader;
var
  nal_unit_type, nal_ref_idc: uint8_t;
begin
  nal_ref_idc := 1;
  if slice.is_idr then
      nal_unit_type := NAL_IDR
  else
      nal_unit_type := NAL_NOIDR;

  write_ue_code(bs, 0);           // first_mb_in_slice   ue(v)
  write_ue_code(bs, slice.type_); // slice_type          ue(v)
  write_ue_code(bs, 0);           // pic_parameter_set_id  ue(v)
  bs.write(slice.frame_num, 4 + sps.log2_max_frame_num_minus4);
  if nal_unit_type = NAL_IDR { 5 } then
      write_ue_code(bs, slice.idr_pic_id); // idr_pic_id 2 ue(v)
  if sps.pic_order_cnt_type = 0 then
    begin
      bs.write(slice.frame_num * 2, 4 + sps.log2_max_pic_order_cnt_lsb_minus4);
      // pic_order_cnt_lsb u(v)
    end;
  if slice.type_ = SLICE_P then
    begin
      // reduce ref count to encoded gop frame count
      if slice.frame_num + 1 > sps.num_ref_frames then
          bs.write(0) // num_ref_idx_active_override_flag  u(1)
      else
        begin
          bs.write(1);
          write_ue_code(bs, slice.frame_num - 1); // num_ref_idx_l0_active_minus1
        end;
    end;
  // ref_pic_list_reordering( )
  if slice.type_ <> SLICE_I then
    begin
      bs.write(0); // ref_pic_list_reordering_flag_l0  u(1)
    end;
  // dec_ref_pic_marking( )
  if nal_ref_idc <> 0 then
    begin
      if nal_unit_type = NAL_IDR then
        begin
          bs.write(0); // no_output_of_prior_pics_flag  u(1)
          bs.write(0); // long_term_reference_flag  u(1)
        end
      else
        begin
          bs.write(0); // adaptive_ref_pic_marking_mode_flag u(1)
        end;
    end;
  write_se_code(bs, slice.slice_qp_delta); // slice_qp_delta se(v)
  if pps.deblocking_filter_control_present_flag > 0 then
    begin
      write_ue_code(bs, 1); // disable_deblocking_filter_idc ue(v)
    end;
end;

{ close slice data bitstream, encapsulate in NAL
}
procedure h264s_write_slice_to_nal(const slice: slice_header_t; var slice_bs, nal_bs: TBitStreamWriter);
var
  nal_unit_type: uint8_t;
begin
  if slice.is_idr then
    begin
      if slice.type_ <> SLICE_I then
          DoStatus('[h264s_write_slice_to_nal] IDR NAL for slicetype <> SLICE_I!');
      nal_unit_type := NAL_IDR;
    end
  else
      nal_unit_type := NAL_NOIDR;
  NAL_encapsulate(slice_bs, nal_bs, nal_unit_type);
end;

procedure TH264Stream.SetChromaQPOffset(const AValue: uint8_t);
begin
  pps.chroma_qp_offset := AValue;
end;

function TH264Stream.GetNoPSkip: Boolean;
begin
  Result := mb_skip_count + 1 > MB_SKIP_MAX;
end;

procedure TH264Stream.SetKeyInterval(const AValue: uint16_t);
begin
  sps.log2_max_frame_num_minus4 := Max(num2log2(AValue) - 4, 0);
  sps.log2_max_pic_order_cnt_lsb_minus4 := sps.log2_max_frame_num_minus4 + 1;
end;

procedure TH264Stream.SetNumRefFrames(const AValue: uint8_t);
begin
  sps.num_ref_frames := AValue;
end;

procedure TH264Stream.SetQP(const AValue: uint8_t);
begin
  pps.qp := AValue;
end;

function TH264Stream.GetSEI: TPascalString;
begin
  Result := sei_string;
end;

procedure TH264Stream.SetSEI(const AValue: TPascalString);
begin
  sei_string := AValue;
end;

(* ******************************************************************************
  mode is derived from surrounding blocks of current or neighboring mbs (if available)

  8.3.1.1 Derivation process for the Intra4x4PredMode

  predIntra4x4PredMode = Min( intra4x4PredModeA, intra4x4PredModeB )
  if( prev_intra4x4_pred_mode_flag[ luma4x4BlkIdx ] )
  Intra4x4PredMode[ luma4x4BlkIdx ] = predIntra4x4PredMode
  else
  if( rem_intra4x4_pred_mode[ luma4x4BlkIdx ] < predIntra4x4PredMode )
  Intra4x4PredMode[ luma4x4BlkIdx ] = rem_intra4x4_pred_mode[ luma4x4BlkIdx ]
  else
  Intra4x4PredMode[ luma4x4BlkIdx ] = rem_intra4x4_pred_mode[ luma4x4BlkIdx ] + 1


  mode = min( pred_mode_A, pred_mode_B )
  if mode = pred_mode_cur
  write pred_mode_flag = 1
  else
  write pred_mode_flag = 0
  if pred_mode < mode
  write mode
  else
  write mode - 1

  a,b idx pair table:
  A
  B Cur
  16   17   18   19
  +-----------------
  20  | 0 |  1 |  4 |  5
  |---+----+----+---
  21  | 2 |  3 |  6 |  7
  |---+----+----+---
  22  | 8 |  9 | 12 | 13
  |---+----+----+---
  23  |10 | 11 | 14 | 15

  (16, 20), (17,  0), ( 0, 21), ( 1,  2),
  (18,  1), (19,  4), ( 4,  3), ( 5,  6),
  ( 2, 21), ( 3,  8), ( 8, 23), ( 9, 10),
  ( 6,  9), ( 7, 12), (12, 11), (13, 14)

*)
function predict_intra_4x4_mode(const modes: array of uint8_t; const i: uint8_t): uint8_t;
const
  idx: array [0 .. 15, 0 .. 1] of uint8_t = (
    (16, 20), (17, 0), (0, 21), (1, 2),
    (18, 1), (19, 4), (4, 3), (5, 6),
    (2, 22), (3, 8), (8, 23), (9, 10),
    (6, 9), (7, 12), (12, 11), (13, 14)
    );
var
  a, b: uint8_t;
begin
  a := modes[idx[i, 0]];
  b := modes[idx[i, 1]];
  if a + b >= INTRA_PRED_NA then
      Result := INTRA_PRED_DC
  else
      Result := Min(a, b);
end;

procedure TH264Stream.write_mb_pred_intra(const mb: TMacroblock);
var
  Mode,          // current block intrapred mode
  pred: uint8_t; // predicted intrapred mode
  i: uint8_t;
begin
  // Luma (MB_I_4x4 only; MB_I_16x16 prediction is derived from mbtype)
  if mb.mbtype = MB_I_4x4 then
    for i := 0 to 15 do
      begin
        pred := predict_intra_4x4_mode(mb.i4_pred_mode, i);
        Mode := mb.i4_pred_mode[i];

        if pred = Mode then
            bs.write(1) // prev_intra4x4_pred_mode_flag[ luma4x4BlkIdx ] 2 u(1) | ae(v)
        else
          begin
            bs.write(0); // prev_intra4x4_pred_mode_flag
            if Mode < pred then
                bs.write(Mode, 3) // rem_intra4x4_pred_mode[ luma4x4BlkIdx ] 2 u(3) | ae(v)
            else
                bs.write(Mode - 1, 3)
          end;
      end;

  // Chroma
  write_ue_code(bs, mb.chroma_pred_mode); // intra_chroma_pred_mode  ue(v)
end;

procedure TH264Stream.write_mb_pred_inter(const mb: TMacroblock);
var
  x, y: int16_t;
begin
  // ref_idx_l0
  case slice.num_ref_frames of
    1:
      ;
    2:
      bs.write(1 - mb.ref); // te() = !value; value = <0,1>
    else
      write_ue_code(bs, mb.ref); // te() = ue()
  end;

  // mvd L0
  x := mb.mv.x - mb.mvp.x;
  y := mb.mv.y - mb.mvp.y;
  write_se_code(bs, x);
  write_se_code(bs, y);
end;

procedure TH264Stream.write_mb_residual(var mb: TMacroblock);
var
  Bits, i: int32_t;
begin
  write_se_code(bs, mb.qp - last_mb_qp); // mb_qp_delta
  last_mb_qp := mb.qp;
  Bits := bs.BitSize;

  // luma
  if mb.mbtype = MB_I_16x16 then
    begin
      cavlc_encode(mb, mb.Block[24], 0, RES_LUMA_DC, bs);
      if (mb.cbp and $F) > 0 then
        for i := 0 to 15 do
            cavlc_encode(mb, mb.Block[i], i, RES_LUMA_AC, bs);
    end
  else
    for i := 0 to 15 do
      if (mb.cbp and (1 shl (i div 4))) > 0 then
          cavlc_encode(mb, mb.Block[i], i, RES_LUMA, bs);

  // chroma
  if mb.cbp shr 4 > 0 then
    begin
      // dc
      for i := 0 to 1 do
          cavlc_encode(mb, mb.Block[25 + i], i, RES_DC, bs);
      // ac
      if mb.cbp shr 5 > 0 then
        begin
          for i := 0 to 3 do
              cavlc_encode(mb, mb.Block[16 + i], i, RES_AC_U, bs);
          for i := 0 to 3 do
              cavlc_encode(mb, mb.Block[16 + 4 + i], i, RES_AC_V, bs);
        end;
    end;

  mb.residual_bits := bs.BitSize - Bits;
end;

constructor TH264Stream.Create(w, h, mbw, mbh: int32_t);
const
  QP_DEFAULT = 26;
begin
  inherited Create;
  sps.width := w;
  sps.height := h;
  sps.mb_width := mbw;
  sps.mb_height := mbh;
  sps.pic_order_cnt_type := 0;
  write_vui := True;
  write_sei := True;
  sei_string := '';

  pps.qp := QP_DEFAULT;
  pps.chroma_qp_offset := 0;
  pps.deblocking_filter_control_present_flag := 0;

  slice.frame_num := 0;
  slice.is_idr := True;
  slice.idr_pic_id := 0;
  slice.type_ := SLICE_I;
  slice.qp := QP_DEFAULT;
  slice.slice_qp_delta := 0;
  slice.num_ref_frames := 1;

  cabac := False;

  interPredCostEval := TH264InterPredCostEvaluator.Create(Self);
end;

destructor TH264Stream.Destroy;
begin
  interPredCostEval.Free;
  inherited Destroy;
end;

procedure TH264Stream.DisableLoopFilter;
begin
  pps.deblocking_filter_control_present_flag := 1;
end;

procedure TH264Stream.InitSlice(slicetype, slice_qp, ref_frame_count: int32_t; bs_buffer: uint8_p);
begin
  bs := TBitStreamWriter.Create(bs_buffer);

  slice.type_ := slicetype;
  slice.qp := slice_qp;
  slice.slice_qp_delta := slice.qp - pps.qp;
  slice.num_ref_frames := ref_frame_count;
  if slice.type_ = SLICE_I then
    begin
      slice.is_idr := True;
      slice.frame_num := 0;
    end
  else
    begin
      slice.is_idr := False;
      inc(slice.frame_num);
    end;

  WriteSliceHeader;

  mb_skip_count := 0;
  last_mb_qp := slice.qp;
end;

procedure TH264Stream.AbortSlice;
begin
  bs.Free;
end;

procedure TH264Stream.GetSliceBitstream(var buffer: uint8_p; out Size: uint32_t);
var
  nalstream: TBitStreamWriter;
begin
  // convert to nal, write sps/pps
  nalstream := TBitStreamWriter.Create(buffer);
  if slice.type_ = SLICE_I then
    begin
      WriteParamSetsToNAL(nalstream);
      if slice.idr_pic_id = 65535 then
          slice.idr_pic_id := 0
      else
          inc(slice.idr_pic_id);
    end;
  h264s_write_slice_to_nal(slice, bs, nalstream);
  nalstream.Close;
  Size := nalstream.ByteSize;

  nalstream.Free;
  bs.Free;
end;

// PCM mb - no compression
procedure TH264Stream.write_mb_i_pcm(var mb: TMacroblock);
var
  i, j, chroma_idx: int32_t;
begin
  // skip run, mbtype
  if slice.type_ = SLICE_P then
    begin
      write_ue_code(bs, mb_skip_count);
      mb_skip_count := 0;
      write_ue_code(bs, 25 + 5);
    end
  else
      write_ue_code(bs, 25); // I_PCM - tab. 7-8

  bs.ByteAlign;
  for i := 0 to 255 do
      bs.write(mb.pixels[i], 8);
  for chroma_idx := 0 to 1 do
    for i := 0 to 7 do
      for j := 0 to 7 do
          bs.write(mb.pixels_c[chroma_idx][i * 16 + j], 8);
end;

procedure TH264Stream.write_mb_i_4x4(var mb: TMacroblock);
begin
  // skip run, mbtype
  if slice.type_ = SLICE_P then
    begin
      write_ue_code(bs, mb_skip_count);
      mb_skip_count := 0;
      write_ue_code(bs, 0 + 5); // I_4x4 in P - tab. 7-10
    end
  else
      write_ue_code(bs, 0); // I_4x4 - tab. 7-8
  // mb_pred
  write_mb_pred_intra(mb);
  // cbp
  write_ue_code(bs, tab_cbp_intra_4x4_to_codenum[mb.cbp]);
  if mb.cbp > 0 then
      write_mb_residual(mb);
end;

{ derive mb_type:
  I_16x16_(pred_mode16)_(cbp_chroma[0..2])_(cbp_luma[0, 15])
}
function mb_I_16x16_mbtype_num(const cbp, pred: int32_t): int32_t; inline;
begin
  Result := 1 + pred + (cbp shr 4) * 4;
  if cbp and $F > 0 then
      inc(Result, 12);
end;

procedure TH264Stream.write_mb_i_16x16(var mb: TMacroblock);
var
  mbt: int32_t;
begin
  mbt := mb_I_16x16_mbtype_num(mb.cbp, mb.i16_pred_mode);
  if slice.type_ = SLICE_P then
    begin
      write_ue_code(bs, mb_skip_count); // skip run
      mb_skip_count := 0;
      write_ue_code(bs, 5 + mbt); // I_16x16 in P - tab. 7-10
    end
  else
      write_ue_code(bs, mbt); // I_16x16 - tab. 7-8
  write_mb_pred_intra(mb);    // mb_pred
  write_mb_residual(mb);
end;

procedure TH264Stream.write_mb_p_16x16(var mb: TMacroblock);
begin
  // skip run, mbtype
  write_ue_code(bs, mb_skip_count);
  mb_skip_count := 0;
  write_ue_code(bs, 0); // P_L0_16x16 - tab. 7-10
  // mb_pred
  write_mb_pred_inter(mb);
  // cbp
  write_ue_code(bs, tab_cbp_inter_4x4_to_codenum[mb.cbp]);
  if mb.cbp > 0 then
      write_mb_residual(mb);
end;

procedure TH264Stream.write_mb_p_skip;
begin
  inc(mb_skip_count);
end;

procedure TH264Stream.WriteMB(var mb: TMacroblock);
begin
  case mb.mbtype of
    MB_I_PCM: write_mb_i_pcm(mb);
    MB_I_4x4: write_mb_i_4x4(mb);
    MB_I_16x16: write_mb_i_16x16(mb);
    MB_P_16x16: write_mb_p_16x16(mb);
    MB_P_SKIP: write_mb_p_skip;
  end;
end;

function TH264Stream.GetBitCost(const mb: TMacroblock): int32_t;
begin
  case mb.mbtype of
    MB_I_4x4: Result := mb_i_4x4_bits(mb);
    MB_I_16x16: Result := mb_i_16x16_bits(mb);
    MB_P_16x16: Result := mb_p_16x16_bits(mb);
    MB_P_SKIP: Result := mb_p_skip_bits;
    else
      Result := 256 + 2 * 64;
  end;
end;

// bitcost functions
function TH264Stream.mb_interpred_bits(const mb: TMacroblock): int32_t;
var
  x, y: int16_t;
begin
  Result := 0;
  case slice.num_ref_frames of
    1:
      ;
    2:
      inc(Result);
    else
      inc(Result, ue_code_len(mb.ref));
  end;
  x := mb.mv.x - mb.mvp.x;
  y := mb.mv.y - mb.mvp.y;
  inc(Result, se_code_len(x) + se_code_len(y));
end;

// Get InterPredCostEvaluator for current slice
function TH264Stream.GetInterPredCostEvaluator: IInterPredCostEvaluator;
begin
  Result := interPredCostEval;
end;

function TH264Stream.mb_intrapred_bits(const mb: TMacroblock): int32_t;
var
  Mode,          // current block intrapred mode
  pred: uint8_t; // predicted intrapred mode
  i: uint8_t;
begin
  Result := 0;
  // Luma
  if mb.mbtype = MB_I_4x4 then
    begin
      Result := 16; // prev_intra4x4_pred_mode_flag
      for i := 0 to 15 do
        begin
          pred := predict_intra_4x4_mode(mb.i4_pred_mode, i);
          Mode := mb.i4_pred_mode[i];

          if pred <> Mode then
              inc(Result, 3);
        end;
    end;
  // Chroma
  inc(Result, ue_code_len(mb.chroma_pred_mode));
end;

function TH264Stream.mb_residual_bits(const mb: TMacroblock): int32_t;
var
  i: uint8_t;
begin
  Result := 0;

  if mb.mbtype = MB_I_16x16 then
    begin
      inc(Result, cavlc_block_bits(mb, mb.Block[24], 0, RES_LUMA_DC));
      if (mb.cbp and $F) > 0 then
        for i := 0 to 15 do
            inc(Result, cavlc_block_bits(mb, mb.Block[i], i, RES_LUMA_AC));
    end
  else
    for i := 0 to 15 do
      if (mb.cbp and (1 shl (i div 4))) > 0 then
          inc(Result, cavlc_block_bits(mb, mb.Block[i], i, RES_LUMA));

  if mb.cbp shr 4 > 0 then
    begin
      for i := 0 to 1 do
          inc(Result, cavlc_block_bits(mb, mb.Block[25 + i], i, RES_DC));
      if mb.cbp shr 5 > 0 then
        begin
          for i := 0 to 3 do
              inc(Result, cavlc_block_bits(mb, mb.Block[16 + i], i, RES_AC_U));
          for i := 0 to 3 do
              inc(Result, cavlc_block_bits(mb, mb.Block[16 + 4 + i], i, RES_AC_V));
        end;
    end;
end;

function TH264Stream.mb_i_4x4_bits(const mb: TMacroblock): int32_t;
begin
  if slice.type_ = SLICE_P then
      Result := ue_code_len(5)
  else
      Result := ue_code_len(0);
  inc(Result, mb_intrapred_bits(mb));
  inc(Result, ue_code_len(tab_cbp_intra_4x4_to_codenum[mb.cbp]));
  if mb.cbp > 0 then
      inc(Result, mb_residual_bits(mb));
end;

function TH264Stream.mb_i_16x16_bits(const mb: TMacroblock): int32_t;
var
  mbt: int32_t;
begin
  mbt := mb_I_16x16_mbtype_num(mb.cbp, mb.i16_pred_mode);
  if slice.type_ = SLICE_P then
      Result := ue_code_len(5 + mbt)
  else
      Result := ue_code_len(mbt);
  inc(Result, mb_intrapred_bits(mb));
  inc(Result, mb_residual_bits(mb));
end;

function TH264Stream.mb_p_16x16_bits(const mb: TMacroblock): int32_t;
begin
  Result := 1 + mb_interpred_bits(mb);
  inc(Result, ue_code_len(tab_cbp_inter_4x4_to_codenum[mb.cbp]));
  if mb.cbp > 0 then
      inc(Result, mb_residual_bits(mb));
end;

function TH264Stream.mb_p_skip_bits: int32_t;
begin
  Result := ue_code_len(mb_skip_count + 1) - ue_code_len(mb_skip_count);
end;

{ TH264InterPredCostEvaluator }

const
  lambda_mv: array [0 .. 51] of uint8_t = (
    0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 2, 2, 2,
    2, 3, 3, 3, 4, 4, 5, 5, 6, 7,
    7, 8, 9, 10, 12, 13, 15, 17, 19, 21,
    23, 26, 30, 33, 37, 42, 47, 53, 59, 66,
    74, 83
    );

procedure TH264InterPredCostEvaluator.SetMVPredAndRefIdx(const mvp: TMotionvec; const idx: int32_t);
begin
  _mvp := mvp;
  _ref_idx := idx;
  case _h264stream.NumRefFrames of
    1: _ref_frame_bits := 0;
    2: _ref_frame_bits := 1;
    else _ref_frame_bits := ue_code_len(_ref_idx);
  end;
end;

constructor TH264InterPredCostEvaluator.Create(const h264stream: TH264Stream);
begin
  _h264stream := h264stream;
  _lambda := 1;
  _mvp := ZERO_MV;
  _ref_idx := 0;
end;

procedure TH264InterPredCostEvaluator.SetQP(qp: int32_t);
begin
  _lambda := lambda_mv[Median(0, qp, 51)];
end;

function TH264InterPredCostEvaluator.bitcost(const mv: TMotionvec): int32_t;
begin
  Result := _ref_frame_bits + se_code_len(mv.x - _mvp.x) + se_code_len(mv.y - _mvp.y);
  Result := Result * _lambda;
end;

end.
