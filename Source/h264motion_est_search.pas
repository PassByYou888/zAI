{ ****************************************************************************** }
{ * h264Motion_est_search.pas        by qq600585                               * }
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

unit h264motion_est_search;

{$INCLUDE zDefine.inc}

interface

uses
  h264Types, h264Common, h264Util, h264Motion_comp, h264Frame;

type
  TRegionSearch = class
  private
    _max_x, _max_y: int32_t;
    _max_x_hpel, _max_y_hpel: int32_t;
    _max_x_qpel, _max_y_qpel: int32_t;
    _last_search_score: int32_t;
    _starting_fpel_mv: TMotionvec;
    MotionCompensator: TMotionCompensation;
    InterCostEval: IInterPredCostEvaluator;
  public
    Cur: uint8_p;
    _mbx, _mby: int32_t;

    property LastSearchScore: int32_t read _last_search_score;

    constructor Create(region_width, region_height: int32_t; mc: TMotionCompensation; cost_eval: IInterPredCostEvaluator);
    procedure PickFPelStartingPoint(const fref: PFrame; const predicted_mv_list: TMotionVectorList);
    function SearchFPel(var mb: TMacroblock; const fref: PFrame): TMotionvec;
    function SearchHPel(var mb: TMacroblock; const fref: PFrame): TMotionvec;
    function SearchQPel(var mb: TMacroblock; const fref: PFrame; const satd, chroma_me: Boolean): TMotionvec;
  end;

implementation

type
  // motion search patterns
  TXYOffs      = array [0 .. 1] of int8_t;
  TMEPrecision = (mpFpel, mpHpel, mpQpel);

const
  FPEL_SAD_TRESH                                 = 64;
  ME_RANGES: array [TMEPrecision] of uint8_t     = (16, 4, 4);
  MIN_XY                                         = -FRAME_EDGE_W;
  MIN_XY_HPEL                                    = MIN_XY * 2;
  MIN_XY_QPEL                                    = MIN_XY * 4;
  pt_dia_small: array [0 .. 3] of TXYOffs        = ((0, -1), (0, 1), (-1, 0), (1, 0));
  pt_dia_large: array [0 .. 7] of TXYOffs        = ((0, -2), (0, 2), (-2, 0), (2, 0), (-1, -1), (-1, 1), (1, -1), (1, 1));
  pt_dia_large_sparse: array [0 .. 3] of TXYOffs = ((0, -2), (0, 2), (-2, 0), (2, 0));
  pt_square: array [0 .. 7] of TXYOffs           = ((0, -1), (0, 1), (-1, 0), (1, 0), (-1, -1), (-1, 1), (1, -1), (1, 1));

constructor TRegionSearch.Create(region_width, region_height: int32_t; mc: TMotionCompensation; cost_eval: IInterPredCostEvaluator);
var
  edge: int32_t;
begin
  MotionCompensator := mc;
  InterCostEval := cost_eval;

  _starting_fpel_mv := ZERO_MV;
  _last_search_score := MaxInt;

  // max. compensated mb position; we need to subtract the unpainted edge
  edge := FRAME_EDGE_W + 1;
  _max_x := region_width - edge;
  _max_y := region_height - edge;

  edge := FRAME_EDGE_W * 2 + 1;
  _max_x_hpel := region_width * 2 - edge;
  _max_y_hpel := region_height * 2 - edge;

  edge := FRAME_EDGE_W * 4 + 1;
  _max_x_qpel := region_width * 4 - edge;
  _max_y_qpel := region_height * 4 - edge;
end;

(*
  Pick a FPel mv that gives the lowest SAD score from a list of predictors.
  This mv will be the starting point for FPel search.
  input
  fref - frame being searched
  me - ME struct set up for fpel
  predicted_mv_list - list of predicted mvs in QPel units
*)
procedure TRegionSearch.PickFPelStartingPoint(const fref: PFrame; const predicted_mv_list: TMotionVectorList);
var
  i, x, y: int32_t;
  stride: int32_t;
  score: int32_t;
  ref: uint8_p;
  tested_mv: TMotionvec;

begin
  ref := fref^.plane_dec[0];
  stride := fref^.stride;

  // test 0,0
  _last_search_score := DSP.sad_16x16(Cur, @ref[stride * _mby + _mbx], stride);
  _starting_fpel_mv := ZERO_MV;

  // test vectors
  for i := 0 to predicted_mv_list.Count - 1 do
    begin
      tested_mv := predicted_mv_list[i] / 4;
      if tested_mv = ZERO_MV then
          Continue;

      x := clip3(MIN_XY, _mbx + tested_mv.x, _max_x);
      y := clip3(MIN_XY, _mby + tested_mv.y, _max_y);
      score := DSP.sad_16x16(Cur, @ref[stride * y + x], stride);

      if score < _last_search_score then
        begin
          _last_search_score := score;
          _starting_fpel_mv := XYToMVec(x - _mbx, y - _mby);
        end;
    end;
end;

(*
  Fullpel ME search using 8-point diamond pattern
  output
  result - best found vector (in qpel units)
*)
function TRegionSearch.SearchFPel(var mb: TMacroblock; const fref: PFrame): TMotionvec;
var
  ref: uint8_p;
  max_x, max_y: int32_t;
  x, y: int32_t; // currently searched fpel x,y position
  stride: int32_t;
  min_score: int32_t;
  mv, mv_prev_pass: TMotionvec;
  ITER: int32_t;
  check_bounds: Boolean;
  Range: int32_t;
  pixel_range: int32_t;

  procedure check_pattern(const Pattern: array of TXYOffs);
  var
    i: int32_t;
    Nx, Ny: int32_t;
    score: int32_t;
  begin
    if check_bounds then
      if (x - 2 < MIN_XY) or (x + 2 > max_x) or
        (y - 2 < MIN_XY) or (y + 2 > max_y) then
          Exit; // use large diamond range
    for i := 0 to length(Pattern) - 1 do
      begin
        Nx := x + Pattern[i][0];
        Ny := y + Pattern[i][1];
        score := DSP.sad_16x16(Cur, @ref[stride * Ny + Nx], stride);
        if score < min_score then
          begin
            min_score := score;
            mv := XYToMVec(Nx - _mbx, Ny - _mby);
          end;
      end;
    x := _mbx + mv.x;
    y := _mby + mv.y;
  end;

begin
  if _last_search_score < FPEL_SAD_TRESH then
    begin
      mb.mv := _starting_fpel_mv * 4;
      Result := mb.mv;
      Exit;
    end;

  ref := fref^.plane_dec[0];
  stride := fref^.stride;
  max_x := _max_x;
  max_y := _max_y;
  Range := ME_RANGES[mpFpel];

  mv := _starting_fpel_mv;
  x := _mbx + mv.x;
  y := _mby + mv.y;
  min_score := _last_search_score;

  ITER := 0;
  pixel_range := 2 * Range + 1;
  check_bounds := (x - pixel_range < MIN_XY) or (x + pixel_range > max_x) or
    (y - pixel_range < MIN_XY) or (y + pixel_range > max_y);
  repeat
    mv_prev_pass := mv;
    check_pattern(pt_dia_large_sparse);
    inc(ITER);
  until (mv = mv_prev_pass) or (ITER >= Range);
  check_pattern(pt_square);
  _last_search_score := min_score;

  // scale mv to qpel units
  mb.mv := mv * 4;
  Result := mb.mv;
end;

(*
  Half-pixel ME search using 4-point diamond pattern
  input
  mb.mv - starting vector in qpel units
  fref^.luma_mc - 4 half-pel interpolated planes
  output
  result - best found vector (in qpel units)
*)
function TRegionSearch.SearchHPel(var mb: TMacroblock; const fref: PFrame): TMotionvec;
var
  ref: array [0 .. 3] of uint8_p;
  max_x, max_y: int32_t;
  mbx, mby,      // macroblock hpel x,y position
  x, y: int32_t; // currently searched hpel x,y position
  stride: int32_t;
  min_score: int32_t;
  mv,
    mv_prev_pass: TMotionvec;
  Range: int32_t;
  ITER: int32_t;
  check_bounds: Boolean;

  procedure check_pattern_hpel();
  var
    i, idx: int32_t;
    Nx, Ny,
      mcx, mcy: int32_t;
    score: int32_t;
  begin
    if check_bounds then
      if (x - 1 < MIN_XY_HPEL) or (x + 1 > max_x) or
        (y - 1 < MIN_XY_HPEL) or (y + 1 > max_y) then
          Exit;
    for i := 0 to 3 do
      begin
        Nx := x + pt_dia_small[i][0];
        Ny := y + pt_dia_small[i][1];

        mcx := Nx div 2;
        mcy := Ny div 2;
        idx := ((Ny and 1) shl 1) or (Nx and 1);
        score := DSP.sad_16x16(Cur, ref[idx] + mcy * stride + mcx, stride)
          + InterCostEval.bitcost(XYToMVec(Nx - mbx, Ny - mby) * 2);

        if score < min_score then
          begin
            min_score := score;
            mv := XYToMVec(Nx - mbx, Ny - mby);
          end;
      end;
    x := mbx + mv.x;
    y := mby + mv.y;
  end;

begin
  for x := 0 to 3 do
      ref[x] := fref^.luma_mc[x];
  stride := fref^.stride;
  mbx := _mbx * 2;
  mby := _mby * 2;
  max_x := _max_x_hpel;
  max_y := _max_y_hpel;
  Range := ME_RANGES[mpHpel];

  // scale to hpel units
  mv := mb.mv / 2;
  x := mbx + mv.x;
  y := mby + mv.y;
  min_score := MaxInt; // we need to include bitcost in score, so reset

  ITER := 0;
  check_bounds := (x - Range < MIN_XY_HPEL) or (x + Range > max_x) or (y - Range < MIN_XY_HPEL) or (y + Range > max_y);
  repeat
    mv_prev_pass := mv;
    check_pattern_hpel;
    inc(ITER);
  until (mv = mv_prev_pass) or (ITER >= Range);
  _last_search_score := min_score;

  // scale mv to qpel units
  mb.mv := mv * 2;
  Result := mb.mv;
end;

(*
  quarter-pixel motion vector refinement - search using 4-point diamond pattern
  input
  me - ME struct set up for qpel
  h.mb.mv - starting vector in qpel units
  output
  h.mb.mv - best found vector in qpel units
*)
function TRegionSearch.SearchQPel(var mb: TMacroblock; const fref: PFrame; const satd, chroma_me: Boolean): TMotionvec;
var
  mbcmp: mbcmp_func_t;
  max_x, max_y: int32_t;
  mbx, mby,      // macroblock qpel x,y position
  x, y: int32_t; // currently searched qpel x,y position
  min_score: int32_t;
  mv,
    mv_prev_pass: TMotionvec;
  Range: int32_t;
  ITER: int32_t;
  check_bounds: Boolean;

  function chroma_score: int32_t;
  begin
    Result := DSP.satd_8x8(mb.pixels_c[0], mb.mcomp_c[0], 16);
    inc(Result, DSP.satd_8x8(mb.pixels_c[1], mb.mcomp_c[1], 16));
  end;

  procedure check_pattern_qpel;
  var
    i: int32_t;
    Nx, Ny,
      score: int32_t;
  begin
    if check_bounds then
      if (x - 1 < MIN_XY_QPEL) or (x + 1 > max_x) or (y - 1 < MIN_XY_QPEL) or (y + 1 > max_y) then
          Exit;
    for i := 0 to 3 do
      begin
        Nx := x + pt_dia_small[i][0];
        Ny := y + pt_dia_small[i][1];

        MotionCompensator.CompensateQPelXY(fref, Nx, Ny, mb.mcomp);
        score := mbcmp(Cur, mb.mcomp, 16) + InterCostEval.bitcost(XYToMVec(Nx - mbx, Ny - mby));

        if chroma_me then
          begin
            MotionCompensator.CompensateChromaQpelXY(fref, Nx, Ny, mb.mcomp_c[0], mb.mcomp_c[1]);
            inc(score, chroma_score());
          end;

        if score < min_score then
          begin
            min_score := score;
            mv := XYToMVec(Nx - mbx, Ny - mby);
          end;
      end;
    x := mbx + mv.x;
    y := mby + mv.y;
  end;

begin
  mbx := _mbx * 4;
  mby := _mby * 4;
  max_x := _max_x_qpel;
  max_y := _max_y_qpel;
  if satd then
      mbcmp := DSP.satd_16x16
  else
      mbcmp := DSP.sad_16x16;
  Range := ME_RANGES[mpQpel];

  mv := mb.mv;
  x := mbx + mv.x;
  y := mby + mv.y;
  min_score := MaxInt; // reset score, mbcmp may be different

  ITER := 0;
  check_bounds := (x - Range < MIN_XY_QPEL) or (x + Range > max_x) or (y - Range < MIN_XY_QPEL) or (y + Range > max_y);
  repeat
    mv_prev_pass := mv;
    check_pattern_qpel();
    inc(ITER);
  until (mv = mv_prev_pass) or (ITER >= Range);

  if min_score = MaxInt then
    begin // return valid score if no searches were done (rare cases at the padded edge of a frame)
      MotionCompensator.CompensateQPelXY(fref, x, y, mb.mcomp);
      min_score := mbcmp(Cur, mb.mcomp, 16) + InterCostEval.bitcost(mv);
      if chroma_me then
        begin
          MotionCompensator.CompensateChromaQpelXY(fref, x, y, mb.mcomp_c[0], mb.mcomp_c[1]);
          inc(min_score, chroma_score());
        end;
    end;

  _last_search_score := min_score;
  mb.mv := mv;
  Result := mv;
end;

end.  
 
 
