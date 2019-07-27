{ ****************************************************************************** }
{ * h264RateControl.pas        by qq600585                                     * }
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

unit h264RateControl;

{$INCLUDE zDefine.inc}

interface

uses
  SysUtils,
{$IFDEF FPC}
  fgl,
{$ENDIF FPC}
  h264Types, h264Common, h264Util,
  CoreClasses;

type
  TBufferState = (bsUnderflow, bsOverflow, bsStable);

type
  TRcFrame = record
    BitSize: int32_t;
    tex_bits: int32_t;
    itex, ptex: int32_t;
    misc_bits: int32_t;
    frame_type: uint8_t;
    qp: uint8_t;
    qp_init: uint8_t;
    mb_i, mb_p, mb_skip: int32_t;
  end;

  PRcFrame = ^TRcFrame;

  TRcGop = class
  private
    procedure AdjustByFrameReferences;
    procedure AdjustByFrameRelativeSize(const avg_bitsize: int32_t);
    procedure AdjustByGopRelativeSize(const avg_bitsize: int32_t);
    procedure GopQPBlur;
  public
    length: int32_t;
    I_only: Boolean;
    frames: array of PRcFrame;

    destructor Destroy; override;
    procedure AdjustRelativeQPs(const avg_bitsize: int32_t; const default_qp: int32_t);
    procedure ShiftQPs(Diff: int32_t);
  end;

  {$IFDEF FPC}
  TRcGopList = specialize TFPGList<TRcGop>;
  {$ELSE FPC}
  TRcGopList = TGenericsList<TRcGop>;
  {$ENDIF FPC}

  TRatecontrol = class
  private
    Mode: uint8_t; // 0 - cqp, 1 - 2nd pass avg. bitrate
    qp_const: uint8_t;
    intra_bonus: uint8_t;
    nframes: int32_t;
    frames: array of TRcFrame;
    gop_list: TRcGopList;
    desired_bitrate: int32_t; // kbps
    fps: Single;
    encoded_qp_avg: Single;
    rate_mult: Single;
    stream_bits_estimated, stream_bits_real: Int64_t;
    last_buffer_check, last_diff: int32_t;
    avg_target_framesize: int32_t;
    bit_reserve: int32_t;
    qp_comp: int32_t;
    buffer_state: TBufferState;
    last_buffer_change: int32_t;
    bit_reserve_reduce_from: int32_t;
    ssd: Int64_t;

    procedure CreateGOPs();
    procedure analyse;
    procedure SetBitReserveReductionPoint;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetConstQP(const ConstQP: uint8_t);
    procedure Set2pass(const TargetBitrate, FrameCount: int32_t; const FramesPS: Single);
    function GetQP(const FrameNum: int32_t; const FrameType: uint8_t): uint8_t;
    function GetFrameType(const FrameNum: int32_t): uint8_t;
    procedure Update(const FrameNum: int32_t; const FrameBits: int32_t; var f: TFrame);
  end;

implementation

// estimate new frame size by 1st pass stats
function RecalculateFrameSize(var frame: TRcFrame): int32_t;
var
  Diff: int32_t;
  bits_ptex, bits_itex, bits_misc: int32_t;
  mult_ptex, mult_itex, mult_misc: int32_t;
begin
  Result := frame.BitSize;
  Diff := frame.qp_init - frame.qp;
  if Diff = 0 then
      Exit;
  if frame.BitSize < 256 then
      Exit;

  if frame.frame_type = SLICE_I then
    begin
      mult_itex := 6;
      if (frame.itex / frame.mb_i) < 10 then
          mult_itex := 12;

      if Diff > 0 then
        begin
          // lower qp -> increase bitrate
          bits_itex := Trunc((Diff / mult_itex) * frame.itex) + frame.itex;
        end
      else
        begin
          // higher qp -> decrease bitrate
          Diff := -Diff;
          bits_itex := Trunc(1 / (1 + Diff / mult_itex) * frame.itex);
        end;
      bits_ptex := 0;
      bits_misc := frame.misc_bits;
    end
  else
    begin
      mult_ptex := 3;
      mult_itex := 6;
      mult_misc := 24;
      if frame.mb_p > 0 then
        begin
          if frame.ptex / frame.mb_p > 100 then
              inc(mult_ptex);
          if frame.ptex / frame.mb_p < 25 then
              dec(mult_ptex);
        end;
      if (frame.mb_i > 0) and (frame.itex / frame.mb_i < 10) then
          mult_itex := 3;

      if Diff > 0 then
        begin
          bits_ptex := Trunc((Diff / mult_ptex) * frame.ptex) + frame.ptex;
          bits_itex := Trunc((Diff / mult_itex) * frame.itex) + frame.itex;
          bits_misc := Trunc((Diff / mult_misc) * frame.misc_bits) + frame.misc_bits;
        end
      else
        begin
          Diff := -Diff;
          bits_ptex := Trunc(1 / (1 + Diff / mult_ptex) * frame.ptex);
          bits_itex := Trunc(1 / (1 + Diff / mult_itex) * frame.itex);
          bits_misc := Trunc(1 / (1 + Diff / mult_misc) * frame.misc_bits);
        end;
    end;
  Result := bits_itex + bits_ptex + bits_misc;
  frame.BitSize := Result;
end;

procedure TRcGop.AdjustRelativeQPs(const avg_bitsize: int32_t; const default_qp: int32_t);
var
  i: int32_t;
begin
  // nonreferenced I frame
  if length = 1 then
    begin
      frames[0]^.qp := default_qp;
      Exit;
    end;

  if I_only then
    begin
      for i := 0 to length - 1 do
          frames[i]^.qp := default_qp;
    end
  else
    begin
      AdjustByGopRelativeSize(avg_bitsize);
      AdjustByFrameRelativeSize(avg_bitsize);
      AdjustByFrameReferences;
    end;

  // nonreferenced last frame penalty
  inc(frames[length - 1]^.qp);
end;

procedure TRcGop.AdjustByGopRelativeSize(const avg_bitsize: int32_t);
var
  i: int32_t;
  BitSize, avg_gop_frame_size: int32_t;
  qp_bonus: int32_t;
begin
  BitSize := 0;
  for i := 1 to length - 1 do
      inc(BitSize, frames[i]^.BitSize);
  avg_gop_frame_size := BitSize div (length - 1);

  qp_bonus := 0;
  if avg_gop_frame_size < avg_bitsize / 2 then
      qp_bonus := -1;
  if avg_gop_frame_size > avg_bitsize / 2 then
      qp_bonus := 1;

  for i := 1 to length - 1 do
      inc(frames[i]^.qp, qp_bonus);
end;

// improve/reduce frames far below/above avg. frame size
procedure TRcGop.AdjustByFrameRelativeSize(const avg_bitsize: int32_t);
const
  MAX_QP_DELTA = 5;
var
  i: int32_t;
  BitSize: int32_t;
  qp, qp_bonus: int32_t;
begin
  // I frame - boost only
  BitSize := frames[0]^.BitSize;
  qp := frames[0]^.qp;
  if BitSize < avg_bitsize / 2 then
    begin
      qp_bonus := avg_bitsize div BitSize * 2;
      qp_bonus := Min(qp_bonus, MAX_QP_DELTA);
      frames[0]^.qp := clip3(10, qp - qp_bonus, 51);
    end;

  // P frames
  for i := 1 to length - 1 do
    begin
      BitSize := frames[i]^.BitSize;
      qp := frames[i]^.qp;
      if BitSize < avg_bitsize / 2 then
        begin
          qp_bonus := avg_bitsize div BitSize;
          qp_bonus := Min(qp_bonus, MAX_QP_DELTA);
          frames[i]^.qp := clip3(10, qp - qp_bonus, 51);
        end;

      if BitSize > avg_bitsize * 2 then
        begin
          qp_bonus := BitSize div avg_bitsize;
          qp_bonus := Min(qp_bonus, MAX_QP_DELTA);
          frames[i]^.qp := clip3(10, qp + qp_bonus, 51);
        end;
    end;
end;

// improve frames if followed by frame with mostly skip MBs
procedure TRcGop.AdjustByFrameReferences;
var
  i, k: int32_t;
  mb_count: int32_t;
  qp_bonus: int32_t;
  qp_bonusf: Single;
begin
  mb_count := frames[0]^.mb_i;
  for i := 0 to length - 2 do
    begin
      k := i + 1;
      qp_bonusf := 0;
      while (k <= length - 1) and (frames[k]^.mb_skip > mb_count div 8 * 7) do
        begin
          qp_bonusf := qp_bonusf + 0.5;
          inc(k);
        end;
      qp_bonus := Min(Trunc(qp_bonusf), 6);
      frames[i]^.qp := clip3(10, int32_t(frames[i]^.qp) - qp_bonus, 51);
    end;
end;

procedure TRcGop.GopQPBlur;
var
  i, j: int32_t;
  tmp: array of uint8_t;
  qp: int32_t;
begin
  SetLength(tmp, length + 4);
  tmp[0] := frames[1]^.qp; // don't let I frame influence the rest
  for i := 1 to length - 1 do
      tmp[i] := frames[i]^.qp;
  for i := length to length + 1 do
      tmp[i] := frames[length - 1]^.qp;

  for i := 2 to length - 1 do
    begin
      qp := 0;
      for j := i - 2 to i + 2 do
          inc(qp, tmp[j]);
      qp := qp div 5;
      frames[i]^.qp := qp;
    end;
  tmp := nil;
end;

destructor TRcGop.Destroy;
begin
  frames := nil;
  inherited Destroy;
end;

procedure TRcGop.ShiftQPs(Diff: int32_t);
var
  i: int32_t;
begin
  for i := 0 to length - 1 do
      frames[i]^.qp := clip3(0, frames[i]^.qp + Diff, 51);
end;

procedure TRatecontrol.CreateGOPs;
var
  i, k: int32_t;
  gop: TRcGop;
  gop_len: int32_t;
begin
  i := 0;
  while i < nframes - 1 do
    begin
      gop := TRcGop.Create;

      gop_len := 0;
      repeat
          inc(gop_len);
      until (i + gop_len >= nframes) or (frames[i + gop_len].frame_type = SLICE_I);
      // in successive I frames case, insert all I frames in one GOP
      if gop_len = 1 then
        begin
          repeat
              inc(gop_len);
          until (i + gop_len >= nframes) or (frames[i + gop_len].frame_type <> SLICE_I);
          // last I belongs to next GOP
          if i + gop_len < nframes then
              dec(gop_len);
          gop.I_only := True;
        end;

      gop.length := gop_len;
      SetLength(gop.frames, gop.length);
      for k := 0 to gop.length - 1 do
          gop.frames[k] := @frames[i + k];
      gop_list.Add(gop);

      inc(i, gop_len);
    end;
end;

procedure TRatecontrol.analyse;
var
  i: int32_t;
  stream_size_total: Int64_t;
  kbps: Single;
  avg_size: int32_t;
  Diff: int32_t;
  qp_avg: Single;
  qp_init: int32_t;
  gop: TRcGop;
  reserve_frames: int32_t;
begin
  // calculate average frame size & average QP from stored stats
  qp_avg := 0;
  stream_size_total := 0;
  for i := 0 to nframes - 1 do
    begin
      inc(stream_size_total, frames[i].BitSize);
      qp_avg := qp_avg + frames[i].qp;
    end;
  qp_avg := qp_avg / nframes;
  avg_size := stream_size_total div nframes;
  kbps := stream_size_total / 1000 / (nframes / fps);

  // redistribute QPs
  CreateGOPs();
  for gop in gop_list do
      gop.AdjustRelativeQPs(avg_size, Round(qp_avg));

  // predict new frame sizes according to modified QPs
  qp_avg := 0;
  stream_size_total := 0;
  for i := 0 to nframes - 1 do
    begin
      inc(stream_size_total, RecalculateFrameSize(frames[i]));
      qp_avg := qp_avg + frames[i].qp;
    end;
  qp_avg := qp_avg / nframes;

  // find QP that would fit the desired filesize
  kbps := stream_size_total / 1000 / (nframes / fps);
  rate_mult := desired_bitrate / kbps;
  if rate_mult > 1 then
    begin
      qp_init := Trunc(qp_avg - (rate_mult - 1) * 5);
    end
  else
    begin
      qp_init := Trunc(qp_avg + (1 / rate_mult - 1) * 5);
    end;
  qp_init := clip3(0, qp_init, 51);
  Diff := Round(qp_init - qp_avg);

  // shift all QPS and predict new frame sizes.
  // Calculate rate compensation, because the sizes won't precisely lead to desired stream size:
  // we would need a perfect frame size predictor and fractional per-frame QPs for that
  for gop in gop_list do
      gop.ShiftQPs(Diff);
  stream_size_total := 0;
  for i := 0 to nframes - 1 do
      inc(stream_size_total, RecalculateFrameSize(frames[i]));
  kbps := stream_size_total / 1000 / (nframes / fps);
  rate_mult := desired_bitrate / kbps;

  // bit_reserve for frame size fluctuations
  avg_target_framesize := Trunc(desired_bitrate * 1000 * (nframes / fps) / nframes);
  reserve_frames := Min(nframes div 100, 3);
  bit_reserve := Trunc(avg_target_framesize * reserve_frames);
  SetBitReserveReductionPoint;
end;

// Try to guess a frame at which we should start reducing bit_reserve to fit the desired stream size
procedure TRatecontrol.SetBitReserveReductionPoint;
var
  remaining_size: int32_t;
  sum_bits: int32_t;
  i: int32_t;
begin
  if bit_reserve = 0 then
    begin
      bit_reserve_reduce_from := nframes;
      Exit;
    end;
  remaining_size := bit_reserve * 30;
  if remaining_size > nframes * avg_target_framesize then
      remaining_size := (nframes - 1) * avg_target_framesize;

  sum_bits := 0;
  i := nframes - 1;
  while (sum_bits < remaining_size) and (i > 0) do
    begin
      inc(sum_bits, Trunc(frames[i].BitSize * rate_mult));
      dec(i);
    end;
  bit_reserve_reduce_from := i;
end;

constructor TRatecontrol.Create;
begin
  inherited Create;
  gop_list := TRcGopList.Create;
  frames := nil;
  qp_const := 22;
  fps := 25;
  intra_bonus := 2;
  desired_bitrate := 2000;
  stream_bits_real := 0;
  stream_bits_estimated := 0;

  last_buffer_check := 0;
  last_diff := 0;
  qp_comp := 0;
  buffer_state := bsStable;
  ssd := 0;
end;

destructor TRatecontrol.Destroy;
var
  gop: TRcGop;
begin
  frames := nil;
  for gop in gop_list do
      gop.Free;
  gop_list.Free;
  inherited Destroy;
end;

procedure TRatecontrol.SetConstQP(const ConstQP: uint8_t);
begin
  qp_const := clip3(0, ConstQP, 51);
  Mode := 0;
end;

procedure TRatecontrol.Set2pass(const TargetBitrate, FrameCount: int32_t; const FramesPS: Single);
begin
  desired_bitrate := TargetBitrate;
  nframes := FrameCount;
  fps := FramesPS;
  Mode := 1;
  // read stats
  SetLength(frames, nframes);
  analyse;
end;

function TRatecontrol.GetQP(const FrameNum: int32_t; const FrameType: uint8_t): uint8_t;
begin
  case Mode of
    0:
      begin
        Result := qp_const;
        if FrameType = SLICE_I then
            Result := Max(Result - intra_bonus, 0);
      end;
    1:
      begin
        Result := clip3(0, frames[FrameNum].qp + qp_comp, 51);
      end;
  end;

  encoded_qp_avg := encoded_qp_avg + Result;
end;

function TRatecontrol.GetFrameType(const FrameNum: int32_t): uint8_t;
begin
  Result := SLICE_P;
  if Mode = 1 then
      Result := frames[FrameNum].frame_type;
end;

procedure TRatecontrol.Update(const FrameNum: int32_t; const FrameBits: int32_t; var f: TFrame);
const
  STATECHECK_INTERVAL = 10;
  REACTION_DELAY = 30;
var
  new_diff: int32_t;
  estimated_framebits: int32_t;
  bits_delta: Int64_t;
begin
  if Mode = 0 then
      Exit;

  estimated_framebits := frames[FrameNum].BitSize;
  inc(stream_bits_estimated, Trunc(estimated_framebits * rate_mult));
  inc(stream_bits_real, FrameBits);
  new_diff := stream_bits_real - stream_bits_estimated;

  bits_delta := estimated_framebits - FrameBits;
  inc(ssd, bits_delta * bits_delta);
  f.estimated_framebits := estimated_framebits;
  f.qp_adj := qp_comp;

  // reduce bitreserve towards stream end
  if (FrameNum = bit_reserve_reduce_from) and (bit_reserve >= avg_target_framesize) then
    begin
      dec(bit_reserve, avg_target_framesize);
      case buffer_state of
        bsUnderflow:
          dec(qp_comp);
        bsOverflow:
          inc(qp_comp);
      end;
      SetBitReserveReductionPoint();
    end;

  if FrameNum - last_buffer_check < STATECHECK_INTERVAL then
      Exit;

  last_buffer_check := FrameNum;

  // check changes in stream
  case buffer_state of

    // check if over/underflow and treat
    bsStable:
      begin
        // underflow
        if stream_bits_real < stream_bits_estimated - bit_reserve then
          begin
            buffer_state := bsUnderflow;
            dec(qp_comp);
            last_buffer_change := FrameNum;
          end;
        if stream_bits_real > stream_bits_estimated + bit_reserve then
          begin
            buffer_state := bsOverflow;
            inc(qp_comp);
            last_buffer_change := FrameNum;
          end;
      end;

    // check underflow state
    bsUnderflow:
      begin
        if new_diff > last_diff then
          begin
            // recovering - do nothing
          end
        else
          begin
            // needs stronger treatment
            if FrameNum - last_buffer_change >= REACTION_DELAY then
              begin
                dec(qp_comp);
                last_buffer_change := FrameNum;
              end;
          end;
        // no more underflow, stabilize
        if stream_bits_real + bit_reserve > stream_bits_estimated then
          begin
            buffer_state := bsStable;
            inc(qp_comp);
          end;
      end;

    // check underflow state
    bsOverflow:
      begin
        if new_diff < last_diff then
          begin
            // recovering - do nothing
          end
        else
          begin
            // needs stronger treatment
            if FrameNum - last_buffer_change >= REACTION_DELAY then
              begin
                inc(qp_comp);
                last_buffer_change := FrameNum;
              end;
          end;
        // no more overflow
        if stream_bits_real < stream_bits_estimated + bit_reserve then
          begin
            buffer_state := bsStable;
            dec(qp_comp);
          end;
      end;
  end;

  last_diff := new_diff;
end;

end.
