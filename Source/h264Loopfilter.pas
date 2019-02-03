{ ****************************************************************************** }
{ * h264Loopfilter.pas        by qq600585                                      * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

unit h264Loopfilter;

{$INCLUDE zDefine.inc}
{$POINTERMATH ON}

interface

uses
  h264Types, h264Common, h264Util, SysUtils, SyncObjs, CoreClasses;

type
  IDeblocker = class
    procedure MBRowFinished; virtual; abstract;
    procedure FrameFinished; virtual; abstract;
  end;

function GetNewDeblocker(const frame: TFrame; const constant_qp, threading_enabled: Boolean): IDeblocker;
procedure CalculateBStrength(const mb: PMacroblock);

implementation

type
  TDeblockThread = class(TCoreClassThread)
  private
    _encoded_mb_rows: int32_t;
    _encoded_mb_rows_lock: TCriticalSection;
    _row_processed_event: TSimpleEvent;
    _abort: Boolean;
    _abort_lock: TCriticalSection;

    function GetAbort: Boolean;
    procedure SetEncodedMBRows(AValue: int32_t);
    function GetEncodedMBRows(): int32_t;

  public
    frame: PFrame;
    cqp: Boolean;

    property EncodedMBRows: int32_t read GetEncodedMBRows write SetEncodedMBRows;
    property Abort: Boolean read GetAbort;

    constructor Create;
    destructor Free;

    procedure Execute; override;
    procedure IncreaseEncodedMBRows;
    procedure AbortProcessing;
  end;

  {
    Deblocks in paralell with encoding; running a few macroblock rows behind the encoding thread
  }
  TThreadedDeblocker = class(IDeblocker)
  private
    dthread: TDeblockThread;
    scheduled_mbrows: int32_t;
    f: PFrame;
    _is_frame_finished: Boolean;
  public
    constructor Create(const frame: TFrame; const cqp: Boolean);
    destructor Destroy; override;
    procedure MBRowFinished; override;
    procedure FrameFinished; override;
  end;

  { TSimpleDeblocker
    Deblocks after the whole frame is encoded
  }
  TSimpleDeblocker = class(IDeblocker)
  private
    f: PFrame;
    scheduled_mbrows: int32_t;
    _cqp: Boolean;
  public
    constructor Create(const frame: TFrame; const cqp: Boolean);
    procedure FrameFinished; override;
    procedure MBRowFinished; override;
  end;

function GetNewDeblocker(const frame: TFrame; const constant_qp, threading_enabled: Boolean): IDeblocker;
begin
  if threading_enabled then
      Result := TThreadedDeblocker.Create(frame, constant_qp)
  else
      Result := TSimpleDeblocker.Create(frame, constant_qp);
end;

const
  // Table 8-14 â€?Derivation of indexA and indexB from offset dependent threshold variables Î± and Î²
  TAB_ALPHA: array [0 .. 51] of uint8_t = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 4, 5, 6, 7, 8, 9, 10, 12, 13,
    15, 17, 20, 22, 25, 28, 32, 36, 40, 45, 50, 56, 63, 71, 80, 90, 101, 113, 127, 144, 162, 182, 203, 226, 255, 255);

  TAB_BETA: array [0 .. 51] of uint8_t = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4,
    6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18);

  // Table 8-15 â€?Value of filter clipping variable tC0 as a function of indexA and bS
  TAB_TC0: array [1 .. 3, 0 .. 51] of uint8_t = (
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 6, 6, 7, 8, 9, 10, 11, 13),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5, 5, 6, 7, 8, 8, 10, 11, 12, 13, 15, 17),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 6, 6, 7, 8, 9, 10, 11, 13, 14, 16, 18, 20, 23, 25));

  XY2IDX: array [0 .. 3, 0 .. 3] of uint8_t = (
    (0, 2, 8, 10),
    (1, 3, 9, 11),
    (4, 6, 12, 14),
    (5, 7, 13, 15));

function Clip(i: int32_t): uint8_t; inline;
begin
  if i > 255 then
      i := 255
  else if i < 0 then
      i := 0;
  Result := uint8_t(i);
end;

{ 8.7.2.1 Derivation process for the luma content dependent boundary filtering strength
  mixedModeEdgeFlag = 0 (prog)
}
procedure CalculateBStrength(const mb: PMacroblock);

// test p/q non-zero coeffs
  function inner_bs(const a: PMacroblock; NA, nb: int32_t): int32_t; inline;
  begin
    Result := 0;
    if a^.nz_coef_cnt[NA] + a^.nz_coef_cnt[nb] > 0 then
        Result := 2;
  end;

  function edge_bs(const a, b: PMacroblock; NA, nb: int32_t; bS_min: int32_t): int32_t; inline;
  begin
    Result := bS_min;
    if a^.nz_coef_cnt[NA] + b^.nz_coef_cnt[nb] > 0 then
        Result := 2;
  end;

// different ref, mv delta >= 4, diff. partitions
  function mb_bs(const a, b: PMacroblock): int32_t; inline;
  begin
    Result := 0;
    if (a^.ref <> b^.ref) or
      ((Abs(a^.mv.x - b^.mv.x) >= 4) or (Abs(a^.mv.y - b^.mv.y) >= 4))
    then
        Result := 1;
  end;

  procedure zero16bytes(p: int64_p); inline;
  begin
    p^ := 0;
    (p + 1)^ := 0;
  end;

const
  intra_bs_vert: TBSarray  = ((4, 4, 4, 4), (3, 3, 3, 3), (3, 3, 3, 3), (3, 3, 3, 3));
  intra_bs_horiz: TBSarray = ((4, 3, 3, 3), (4, 3, 3, 3), (4, 3, 3, 3), (4, 3, 3, 3));

var
  i, j: int32_t;
  mba, mbb: PMacroblock;
  bS_min: int32_t;

begin
  if is_intra(mb^.mbtype) then
    begin
      mb^.bS_vertical := intra_bs_vert;
      mb^.bS_horizontal := intra_bs_horiz;
      Exit;
    end;

  // internal edges
  if (mb^.mbtype = MB_P_SKIP) or (mb^.cbp = 0) then
    begin
      zero16bytes(@mb^.bS_vertical);
      zero16bytes(@mb^.bS_horizontal);
    end
  else
    begin
      for i := 1 to 3 do
        for j := 0 to 3 do
            mb^.bS_vertical[i, j] := inner_bs(mb, XY2IDX[i, j], XY2IDX[i - 1, j]);
      for i := 0 to 3 do
        for j := 1 to 3 do
            mb^.bS_horizontal[i, j] := inner_bs(mb, XY2IDX[i, j], XY2IDX[i, j - 1]);
    end;

  // vertical edges - left edge
  if mb^.x > 0 then
    begin
      mba := mb^.mba;
      if is_intra(mba^.mbtype) then
        begin // edge shared with intra block
          for i := 0 to 3 do
              mb^.bS_vertical[0, i] := 4;
        end
      else
        begin
          bS_min := mb_bs(mb, mba);
          mb^.bS_vertical[0, 0] := edge_bs(mb, mba, 0, 5, bS_min);
          mb^.bS_vertical[0, 1] := edge_bs(mb, mba, 2, 7, bS_min);
          mb^.bS_vertical[0, 2] := edge_bs(mb, mba, 8, 13, bS_min);
          mb^.bS_vertical[0, 3] := edge_bs(mb, mba, 10, 15, bS_min);
        end;
    end;

  // horizontal edges - top edge
  if mb^.y > 0 then
    begin
      mbb := mb^.mbb;
      if is_intra(mbb^.mbtype) then
        begin // edge shared with intra block
          for i := 0 to 3 do
              mb^.bS_horizontal[i, 0] := 4;
        end
      else
        begin
          bS_min := mb_bs(mb, mbb);
          mb^.bS_horizontal[0, 0] := edge_bs(mb, mbb, 0, 10, bS_min);
          mb^.bS_horizontal[1, 0] := edge_bs(mb, mbb, 1, 11, bS_min);
          mb^.bS_horizontal[2, 0] := edge_bs(mb, mbb, 4, 14, bS_min);
          mb^.bS_horizontal[3, 0] := edge_bs(mb, mbb, 5, 15, bS_min);
        end;
    end;
end;

procedure DeblockMBRow(
  const mby: int32_t;
  const f: TFrame;
  const cqp: Boolean = True;
  const offset_a: int32_t = 0; const offset_b: int32_t = 0);
var
  p, q: array [0 .. 3] of int32_t;
  bS_vertical, bS_horizontal: TBSarray;
  filterLeftMbEdgeFlag, filterTopMbEdgeFlag: Boolean;

  procedure FilterSamplesLuma(const Strength, indexA, alpha, beta: int32_t);
  var
    tc, tc0: int32_t;
    Delta, d: int32_t;
    ap, aq: int32_t;
    pf, qf: array [0 .. 2] of int32_t;
    i: int32_t;
  begin
    ap := Abs(p[2] - p[0]);
    aq := Abs(q[2] - q[0]);

    // 8.7.2.3 Filtering process for edges with bS less than 4
    if Strength < 4 then
      begin
        tc0 := TAB_TC0[Strength, indexA];
        tc := tc0;
        if ap < beta then
            inc(tc);
        if aq < beta then
            inc(tc);

        // Î” = Clip3( â€“tC, tC, ( ( ( ( q0 â€?p0 ) << 2 ) + ( p1 â€?q1 ) + 4 ) >> 3 ) )
        Delta := SAR32(((q[0] - p[0]) shl 2) + (p[1] - q[1]) + 4, 3);
        Delta := clip3(-tc, Delta, tc);

        // p'1 = p1 + Clip3( â€“tC0, tC0, ( p2 + ( ( p0 + q0 + 1 ) >> 1 ) â€?( p1 << 1 ) ) >> 1 )
        if ap < beta then
          begin
            d := SAR32(p[2] + ((p[0] + q[0] + 1) shr 1) - (p[1] shl 1), 1);
            p[1] := p[1] + clip3(-tc0, d, tc0);
          end;
        // q'1 = q1 + Clip3( â€“tC0, tC0, ( q2 + ( ( p0 + q0 + 1 ) >> 1 ) â€?( q1 << 1 ) ) >> 1 )
        if aq < beta then
          begin
            d := SAR32(q[2] + ((p[0] + q[0] + 1) shr 1) - (q[1] shl 1), 1);
            q[1] := q[1] + clip3(-tc0, d, tc0);
          end;

        // p0, q0
        p[0] := Clip(p[0] + Delta);
        q[0] := Clip(q[0] - Delta);
      end
      // Filtering process for edges for bS equal to 4
    else
      begin
        // ap < Î² && Abs( p0 â€?q0 ) < ( ( Î± >> 2 ) + 2 )
        if (ap < beta) and (Abs(p[0] - q[0]) < (alpha shr 2 + 2)) then
          begin
            pf[0] := (p[2] + 2 * p[1] + 2 * p[0] + 2 * q[0] + q[1] + 4) shr 3;
            pf[1] := (p[2] + p[1] + p[0] + q[0] + 2) shr 2;
            pf[2] := (2 * p[3] + 3 * p[2] + p[1] + p[0] + q[0] + 4) shr 3
          end
        else
          begin
            pf[0] := (2 * p[1] + p[0] + q[1] + 2) shr 2;
            pf[1] := p[1];
            pf[2] := p[2];
          end;

        if (aq < beta) and (Abs(p[0] - q[0]) < (alpha shr 2 + 2)) then
          begin
            qf[0] := (q[2] + 2 * q[1] + 2 * q[0] + 2 * p[0] + p[1] + 4) shr 3;
            qf[1] := (q[2] + q[1] + q[0] + p[0] + 2) shr 2;
            qf[2] := (2 * q[3] + 3 * q[2] + q[1] + q[0] + p[0] + 4) shr 3
          end
        else
          begin
            qf[0] := (2 * q[1] + q[0] + p[1] + 2) shr 2;
            qf[1] := q[1];
            qf[2] := q[2];
          end;

        for i := 0 to 2 do
          begin
            p[i] := pf[i];
            q[i] := qf[i];
          end;
      end;
  end;

  procedure FilterSamplesChroma(const Strength, indexA_c: int32_t);
  var
    tc: int32_t;
    Delta: int32_t;
  begin
    // 8.7.2.3 Filtering process for edges with bS less than 4
    if Strength < 4 then
      begin
        tc := TAB_TC0[Strength, indexA_c] + 1;
        // Î” = Clip3( â€“tC, tC, ( ( ( ( q0 â€?p0 ) << 2 ) + ( p1 â€?q1 ) + 4 ) >> 3 ) )
        Delta := SAR32(((q[0] - p[0]) shl 2) + (p[1] - q[1]) + 4, 3);
        Delta := clip3(-tc, Delta, tc);
        // p0, q0
        p[0] := Clip(p[0] + Delta);
        q[0] := Clip(q[0] - Delta);
      end
      // Filtering process for edges for bS equal to 4
    else
      begin
        p[0] := (2 * p[1] + p[0] + q[1] + 2) shr 2;
        q[0] := (2 * q[1] + q[0] + p[1] + 2) shr 2;
      end;
  end;

  function UseFilter(alpha, beta: int32_t): Boolean;
  begin
    Result := (Abs(p[0] - q[0]) < alpha)
      and (Abs(p[1] - p[0]) < beta)
      and (Abs(q[1] - q[0]) < beta);
  end;

  procedure FilterLuma16x16(const Pixel: uint8_p; const indexA, alpha, beta: int32_t);
  var
    edge, blk, samples: int32_t;
    i: int32_t;
    bs: int32_t;
    starting_edge: int32_t;
    pix: uint8_p;
    stride: int32_t;
  begin
    stride := f.stride;

    // verticals  - edge = x, blk = y
    starting_edge := 0;
    if not filterLeftMbEdgeFlag then
        inc(starting_edge);

    for edge := starting_edge to 3 do
      begin
        pix := Pixel + edge * 4;

        for blk := 0 to 3 do
          begin
            bs := bS_vertical[edge, blk];
            if bs = 0 then
              begin
                inc(pix, 4 * f.stride);
                Continue;
              end;

            for samples := 0 to 3 do
              begin
                for i := 0 to 3 do
                    q[i] := pix[i];
                for i := 0 to 3 do
                    p[i] := pix[-(i + 1)];

                if UseFilter(alpha, beta) then
                  begin
                    FilterSamplesLuma(bs, indexA, alpha, beta);
                    for i := 0 to 2 do
                        pix[i] := q[i];
                    for i := 0 to 2 do
                        pix[-(i + 1)] := p[i];
                  end;

                inc(pix, stride); // next pixel row
              end;
          end;
      end;

    // horizontals  - edge = y, blk = x
    starting_edge := 0;
    if not filterTopMbEdgeFlag then
        inc(starting_edge);

    for edge := starting_edge to 3 do
      begin
        pix := Pixel + edge * 4 * stride;

        for blk := 0 to 3 do
          begin
            bs := bS_horizontal[blk, edge];
            if bs = 0 then
              begin
                inc(pix, 4);
                Continue;
              end;

            for samples := 0 to 3 do
              begin
                for i := 0 to 3 do
                    q[i] := pix[i * stride];
                for i := 0 to 3 do
                    p[i] := pix[-(i + 1) * stride];

                if UseFilter(alpha, beta) then
                  begin
                    FilterSamplesLuma(bs, indexA, alpha, beta);
                    for i := 0 to 2 do
                        pix[i * stride] := q[i];
                    for i := 0 to 2 do
                        pix[-(i + 1) * stride] := p[i];
                  end;

                inc(pix);
              end;
          end;
      end;
  end;

  procedure FilterChroma8x8(const Pixel: uint8_p; const indexA_c, alpha_c, beta_c: int32_t);
  var
    edge, blk, samples: int32_t;
    i: int32_t;
    starting_edge: int32_t;
    bs: int32_t;
    pix: uint8_p;
    stride: int32_t;
  begin
    stride := f.stride_c;

    // verticals  - edge = x, blk = y
    starting_edge := 0;
    if not filterLeftMbEdgeFlag then
        inc(starting_edge);

    for edge := starting_edge to 1 do
      begin
        pix := Pixel + edge * 4;

        for blk := 0 to 1 do
          begin
            for samples := 0 to 3 do
              begin
                for i := 0 to 1 do
                    q[i] := pix[i];
                for i := 0 to 1 do
                    p[i] := pix[-(i + 1)];
                bs := bS_vertical[edge, blk * 2 + samples div 2];

                if (bs > 0) and UseFilter(alpha_c, beta_c) then
                  begin
                    FilterSamplesChroma(bs, indexA_c);
                    pix[0] := q[0];
                    pix[-1] := p[0];
                  end;

                inc(pix, stride); // next pixel row
              end;
          end;
      end;

    // horizontals  - edge = y, blk = x
    starting_edge := 0;
    if not filterTopMbEdgeFlag then
        inc(starting_edge);

    for edge := starting_edge to 1 do
      begin
        pix := Pixel + edge * 4 * stride;

        for blk := 0 to 1 do
          begin
            for samples := 0 to 3 do
              begin
                for i := 0 to 1 do
                    q[i] := pix[i * stride];
                for i := 0 to 1 do
                    p[i] := pix[-(i + 1) * stride];
                bs := bS_horizontal[blk * 2 + samples div 2, edge];

                if (bs > 0) and UseFilter(alpha_c, beta_c) then
                  begin
                    FilterSamplesChroma(bs, indexA_c);
                    pix[0] := q[0];
                    pix[-stride] := p[0];
                  end;

                inc(pix);
              end;
          end;
      end;
  end;

var
  mbx: int32_t;
  indexA, indexA_c: int32_t;
  alpha, beta: int32_t;
  alpha_c, beta_c: int32_t;

  procedure SetupParams(const mb: PMacroblock);
  var
    qp, qpc: int32_t;
    indexB, indexB_c: int32_t;
  begin
    qp := mb^.qp;
    indexA := clip3(0, qp + offset_a, 51);
    indexB := clip3(0, qp + offset_b, 51);
    alpha := TAB_ALPHA[indexA];
    beta := TAB_BETA[indexB];

    if qp < 30 then
      begin
        indexA_c := indexA;
        alpha_c := alpha;
        beta_c := beta;
      end
    else
      begin
        qpc := mb^.qpc;
        indexA_c := clip3(0, qpc + offset_a, 51);
        indexB_c := clip3(0, qpc + offset_b, 51);
        alpha_c := TAB_ALPHA[indexA_c];
        beta_c := TAB_BETA[indexB_c];
      end;
  end;

  procedure FilterMB(const mb: PMacroblock);
  begin
    bS_vertical := mb^.bS_vertical;
    bS_horizontal := mb^.bS_horizontal;
    FilterLuma16x16(uint8_p(mb^.pfdec), indexA, alpha, beta);
    FilterChroma8x8(uint8_p(mb^.pfdec_c[0]), indexA_c, alpha_c, beta_c);
    FilterChroma8x8(uint8_p(mb^.pfdec_c[1]), indexA_c, alpha_c, beta_c);
  end;

var
  mb: PMacroblock;

begin
  if cqp then
    begin
      // DeblockMBRow params are the same for all mbs
      mb := @f.mbs[0];
      SetupParams(mb);
      filterTopMbEdgeFlag := mby > 0;
      for mbx := 0 to f.mbw - 1 do
        begin
          filterLeftMbEdgeFlag := mbx > 0;
          mb := @f.mbs[mby * f.mbw + mbx];
          FilterMB(mb);
        end;
    end
  else
    begin
      // DeblockMBRow params change according to current mb's qp
      filterTopMbEdgeFlag := mby > 0;
      for mbx := 0 to f.mbw - 1 do
        begin
          filterLeftMbEdgeFlag := mbx > 0;
          mb := @f.mbs[mby * f.mbw + mbx];
          SetupParams(mb);
          FilterMB(mb);
        end;
    end;
end;

{ TDeblockThread }

procedure TDeblockThread.SetEncodedMBRows(AValue: int32_t);
begin
  _encoded_mb_rows_lock.Acquire;
  _encoded_mb_rows := AValue;
  _encoded_mb_rows_lock.Release;
end;

function TDeblockThread.GetAbort: Boolean;
begin
  _abort_lock.Acquire;
  Result := _abort;
  _abort_lock.Release;
end;

function TDeblockThread.GetEncodedMBRows: int32_t;
begin
  _encoded_mb_rows_lock.Acquire;
  Result := _encoded_mb_rows;
  _encoded_mb_rows_lock.Release;
end;

constructor TDeblockThread.Create;
begin
  inherited Create(True);

  _row_processed_event := TSimpleEvent.Create;
  _encoded_mb_rows_lock := TCriticalSection.Create;
  _abort_lock := TCriticalSection.Create;

  _abort := False;
  EncodedMBRows := 0;
end;

destructor TDeblockThread.Free;
begin
  _row_processed_event.Free;
  _encoded_mb_rows_lock.Free;
  _abort_lock.Free;
end;

procedure TDeblockThread.Execute;
var
  mby: int32_t;
  row_deblock_limit: int32_t;
  encoded_rows: int32_t;

begin
  mby := 0;

  while mby < frame^.mbh do
    begin
      _row_processed_event.WaitFor(Infinite);
      _row_processed_event.ResetEvent;
      if Abort then
          Break;

      encoded_rows := EncodedMBRows;
      if encoded_rows < frame^.mbh then
          row_deblock_limit := encoded_rows - 2
      else
          row_deblock_limit := frame^.mbh;

      // run in a loop: several rows may have been decoded since the last run (event was set multiple times),
      // or the frame is fully decoded
      while (mby < row_deblock_limit) do
        begin
          DeblockMBRow(mby, frame^, cqp);
          inc(mby);
        end;
    end;
end;

procedure TDeblockThread.IncreaseEncodedMBRows;
begin
  _encoded_mb_rows_lock.Acquire;
  inc(_encoded_mb_rows);
  _encoded_mb_rows_lock.Release;
  _row_processed_event.SetEvent;
end;

procedure TDeblockThread.AbortProcessing;
begin
  _abort_lock.Acquire;
  _abort := True;
  _abort_lock.Release;
  // thread must receive the event to resume its loop and be able to process the abort command
  _row_processed_event.SetEvent;
end;

constructor TThreadedDeblocker.Create(const frame: TFrame; const cqp: Boolean);
begin
  f := @frame;
  scheduled_mbrows := 0;
  _is_frame_finished := False;

  dthread := TDeblockThread.Create;
  dthread.frame := f;
  dthread.cqp := cqp;

  dthread.Start;
end;

destructor TThreadedDeblocker.Destroy;
begin
  if not _is_frame_finished then
      FrameFinished;
  dthread.Free;
end;

procedure TThreadedDeblocker.MBRowFinished;
begin
  inc(scheduled_mbrows);
  dthread.IncreaseEncodedMBRows;
end;

procedure TThreadedDeblocker.FrameFinished;
begin
  if scheduled_mbrows < f^.mbh then
      dthread.AbortProcessing;
  dthread.WaitFor;
  _is_frame_finished := True;
end;

constructor TSimpleDeblocker.Create(const frame: TFrame; const cqp: Boolean);
begin
  f := @frame;
  _cqp := cqp;
  scheduled_mbrows := 0;
end;

procedure TSimpleDeblocker.FrameFinished;
var
  mby: int32_t;
begin
  if scheduled_mbrows = f^.mbh then
    for mby := 0 to f^.mbh - 1 do
      begin
        DeblockMBRow(mby, f^, _cqp);
      end;
end;

procedure TSimpleDeblocker.MBRowFinished;
begin
  inc(scheduled_mbrows);
end;

end.   
 
 
