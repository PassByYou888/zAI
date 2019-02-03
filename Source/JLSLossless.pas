{ ****************************************************************************** }
{ * JPEG-LS Codec https://github.com/zekiguven/pascal_jls                      * }
{ * fixed by QQ 600585@qq.com                                                  * }
{ ****************************************************************************** }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
{
  JPEG-LS Codec
  This code is based on http://www.stat.columbia.edu/~jakulin/jpeg-ls/mirror.htm
  Converted from C to Pascal. 2017

  https://github.com/zekiguven/pascal_jls

  author : Zeki Guven
}
unit JLSLossless;

{$INCLUDE zDefine.inc}

interface

uses JLSGlobal, JLSJpegmark, JLSMelcode, JLSBitIO, CoreClasses;

type
  TJLSLossless = class
  private
    FBitIO: TJLSBitIO;
    FMelcode: TJLSMelcode;
    FImageInfo: PImageInfo;
    eor_limit: Int;
    procedure Clip(var x: Int; alpha: Int);
  public
    constructor Create(ABitIO: TJLSBitIO; AMelcode: TJLSMelcode; AImageInfo: PImageInfo);
    function lossless_regular_mode_d(q, Sign, Px: Int): Int;
    procedure lossless_regular_mode_e(q, Sign, Px: Int; xp: ppixel);
    function lossless_end_of_run_d(RA, rb: Pixel; RItype: Int): Pixel;
    procedure lossless_end_of_run_e(RA, rb, ix: Pixel; RItype: Int);
    function lossless_undoscanline(psl: ppixelarray; { previous scanline }
      SL: ppixelarray;                               { current scanline }
      no: Int; COLOR: Int): Int;

    procedure lossless_doscanline(psl: ppixelarray; { previous scanline }
      SL: ppixelarray;                              { current scanline }
      no, COLOR: Int);                              { number of values in it }
    { For pixel interleaved mode for LOSSLESS encoding }

    procedure lossless_doscanline_pixel(psl: ppixelarray; { previous scanline }
      SL: ppixelarray;                                    { current scanline }
      no: Int);                                           { number of values in it }

    { For DEOCODING pixel interleavde mode for LOSSLESS images }
    function lossless_undoscanline_pixel(psl: ppixelarray; { previous scanline }
      SL: ppixelarray;                                     { current scanline }
      no: Int): Int;                                       { number of values in it }

  end;

implementation


{ clipping macro }
procedure TJLSLossless.Clip(var x: Int; alpha: Int);
begin
  if IsTrue(x and FImageInfo^.highmask) then
    begin
      if (x < 0) then
          x := 0
      else
          x := alpha - 1;
    end;
end;

{ Do Golomb-Rice statistics and DECODING for LOSSLESS images }
function TJLSLossless.lossless_regular_mode_d(q, Sign, Px: Int): Int;
var
  at, Bt, Nt, nst, Errval, absErrval: Int;
  Current, k: Int;
  Temp: Int;
  UTemp: ulong;
begin
  { This function is called only for regular contexts.
    End_of_run context is treated separately }

  Nt := FImageInfo^.n[q];
  at := FImageInfo^.a[q];

  { Estimate k }
  k := 0;
  nst := Nt;
  while (nst < at) do
    begin
      nst := nst * 2;
      inc(k);
    end;

  { Get the number of leading zeros }
  absErrval := 0;
  while True do
    begin
      Temp := FBitIO.zeroLUT[shr_c(FBitIO.reg, 24)];
      absErrval := absErrval + Temp;
      if (Temp <> 8) then
        begin
          FBitIO.fillbuffer(Temp + 1);
          Break;
        end;
      FBitIO.fillbuffer(8);
    end;

  if (absErrval < FImageInfo^.Limit) then
    begin
      { now add the binary part of the Rice code }
      if IsTrue(k) then
        begin
          absErrval := absErrval shl k;
          UTemp := shr_c(FBitIO.reg, (32 - k));
          FBitIO.fillbuffer(k);
          absErrval := absErrval + UTemp;
        end;
    end
  else begin
      { the original unary would have been too long:
        (mapped value)-1 was sent verbatim }
      absErrval := shr_c(FBitIO.reg, 32 - (FImageInfo^.qbpp));
      FBitIO.fillbuffer(FImageInfo^.qbpp);

      inc(absErrval);
    end;

  { Do the Rice mapping }
  if IsTrue(absErrval and 1) then
    begin { negative }
      absErrval := (absErrval + 1) div 2;
      Errval := -absErrval;
    end
  else
    begin
      absErrval := absErrval div 2;
      Errval := absErrval;
    end;

  Bt := FImageInfo^.b[q];

  if ((k = 0) and (2 * Bt <= -Nt)) then
    begin
      { special case: see encoder side }
      Errval := -(Errval + 1);

      if (Errval < 0)
      then
          absErrval := -Errval
      else
          absErrval := Errval;
    end;

  { center, clip if necessary, and mask final error }
  if (Sign = -1) then
    begin
      Px := Px - FImageInfo^.c[q];
      Clip(Px, FImageInfo^.alpha);
      { this is valid if alpha is a power of 2 }
      Current := (Px - Errval) and (FImageInfo^.alpha - 1);
    end
  else begin
      Px := Px + FImageInfo^.c[q];
      Clip(Px, FImageInfo^.alpha);
      { valid if alpha is a power of 2 }
      Current := (Px + Errval) and (FImageInfo^.alpha - 1);
    end;

  { update bias stats }
  Bt := Bt + Errval;
  FImageInfo^.b[q] := Bt;

  { update Golomb-Rice stats }
  FImageInfo^.a[q] := FImageInfo^.a[q] + absErrval;

  { check reset (joint for Rice-Golomb and bias cancelation) }
  if (Nt = FImageInfo^.Reset) then
    begin
      Nt := shr_c(Nt, 1);
      FImageInfo^.n[q] := Nt;
      FImageInfo^.a[q] := shr_c(FImageInfo^.a[q], 1);
      Bt := shr_c(Bt, 1);
      FImageInfo^.b[q] := Bt;
    end;

  { Do bias estimation for NEXT pixel }
  inc(Nt);
  FImageInfo^.n[q] := Nt;
  if (Bt <= -Nt) then
    begin

      if (FImageInfo^.c[q] > MIN_C) then
          dec(FImageInfo^.c[q]);
      FImageInfo^.b[q] := FImageInfo^.b[q] + Nt;
      Bt := FImageInfo^.b[q];

      if (Bt <= -Nt) then
          FImageInfo^.b[q] := -Nt + 1;

    end
  else
    if (Bt > 0) then
    begin

      if (FImageInfo^.c[q] < MAX_C) then
          inc(FImageInfo^.c[q]);
      FImageInfo^.b[q] := FImageInfo^.b[q] - Nt;
      Bt := FImageInfo^.b[q];

      if (Bt > 0) then
          FImageInfo^.b[q] := 0;
    end;

  Result := Current;
end;

{ Do end of run DECODING for LOSSLESS images }
function TJLSLossless.lossless_end_of_run_d(RA, rb: Pixel; RItype: Int): Pixel;
var
  ix,
    Errval,
    absErrval,
    MErrval,
    k,
    q,
    oldmap,
    Nt,
    at: Int;
  Temp: Int;
  UTemp: ulong;
begin
  q := EOR_0 + RItype;
  Nt := FImageInfo^.n[q];
  at := FImageInfo^.a[q];

  if IsTrue(RItype) then
      at := at + Nt div 2;

  { Estimate k }
  k := 0;
  while (Nt < at) do
    begin
      Nt := Nt * 2;
      inc(k)
    end;

  { read and decode the Golomb code }
  { Get the number of leading zeros }
  MErrval := 0;
  while True do
    begin
      Temp := FBitIO.zeroLUT[shr_c(FBitIO.reg, 24)];
      MErrval := MErrval + Temp;
      if (Temp <> 8) then
        begin
          FBitIO.fillbuffer(Temp + 1);
          Break;
        end;

      FBitIO.fillbuffer(8);
    end;

  eor_limit := FImageInfo^.Limit - FImageInfo^.limit_reduce;

  if (MErrval < eor_limit) then
    begin
      { now add the binary part of the Golomb code }
      if IsTrue(k) then
        begin
          MErrval := MErrval shl k;
          UTemp := shr_c(FBitIO.reg, 32 - (k));
          FBitIO.fillbuffer(k);
          MErrval := MErrval + UTemp;
        end;
    end
  else
    begin
      { the original unary would have been too long:
        (mapped value)-1 was sent verbatim }
      MErrval := shr_c(FBitIO.reg, 32 - (FImageInfo^.qbpp));
      FBitIO.fillbuffer(FImageInfo^.qbpp);

      inc(MErrval);
    end;

  if ((k = 0) and IsTrue(RItype or MErrval) and (2 * FImageInfo^.b[q] < Nt)) then
      oldmap := 1
  else
      oldmap := 0;
  {
    Note: the Boolean variable 'oldmap' is not
    identical to the variable 'map' in the
    JPEG-LS draft. We have
    oldmap = (qdiff<0) ? (1-map) : map;
  }

  MErrval := MErrval + (RItype + oldmap);

  if IsTrue(MErrval and 1) then
    begin { negative }
      Errval := oldmap - (MErrval + 1) div 2;
      absErrval := -Errval - RItype;
      inc(FImageInfo^.b[q]);
    end
  else begin { nonnegative }
      Errval := MErrval div 2;
      absErrval := Errval - RItype;
    end;

  if (rb < RA) then
      ix := (rb - Errval) and (FImageInfo^.alpha - 1)
  else { includes case a==b }
      ix := (rb + Errval) and (FImageInfo^.alpha - 1);

  { update stats }
  FImageInfo^.a[q] := FImageInfo^.a[q] + absErrval;
  if (FImageInfo^.n[q] = FImageInfo^.Reset) then
    begin
      FImageInfo^.n[q] := shr_c(FImageInfo^.n[q], 1);
      FImageInfo^.a[q] := shr_c(FImageInfo^.a[q], 1);
      FImageInfo^.b[q] := shr_c(FImageInfo^.b[q], 1);
    end;

  inc(FImageInfo^.n[q]); { for next pixel }

  Result := ix;
end;

function TJLSLossless.lossless_undoscanline(psl: ppixelarray; { previous scanline }
  SL: ppixelarray;                                            { current scanline }
  no: Int; COLOR: Int): Int;
var
  i, psfix: Int;
  Diff: Int;
  RA, rb, RC, Rd: Pixel;
  Sign: Int;
  cont: Int;
  Px: Pixel;
  n, M: Int;
  MinX, MaxX: Pixel;
begin
  psfix := 0;

  { **********************************************/
    /* Do for all pixels in the row in 8-bit mode */
    /********************************************** }
  if (FImageInfo^.bpp16 = False) then
    begin

      RC := psl^[0];
      rb := psl^[1];
      RA := SL^[0];

      i := 1;

      repeat

        Rd := psl^[i + 1];

        { Quantize the gradient }
        cont := FImageInfo^.vLUT[0][Rd - rb + LUTMAX8] +
          FImageInfo^.vLUT[1][rb - RC + LUTMAX8] +
          FImageInfo^.vLUT[2][RC - RA + LUTMAX8];

        if (cont = 0) then
          begin
            { *********** RUN STATE ********** }

            { get length of the run }
            { arg is # of pixels left }
            M := FMelcode.process_run_dec(no - i + 1, COLOR);
            n := M;

            if (M > 0) then
              begin { run of nonzero length, otherwise
                  we go directly to the end-of-run
                  state }
                repeat
                  SL^[i] := RA;
                  inc(i);
                  dec(n);
                until not(n > 0);

                if (i > no) then { end of line }
                  begin
                    Result := 0;
                    Exit;
                  end;
                { update context pixels }
                rb := psl^[i];
                Rd := psl^[i + 1];
              end;

            { Do end of run encoding for LOSSLESS images }
            if (RA = rb) then
                RA := lossless_end_of_run_d(RA, rb, 1)
            else
                RA := lossless_end_of_run_d(RA, rb, 0);

          end { Run state block }
        else
          begin
            { ************ REGULAR CONTEXT ********** }

            Px := predict(rb, RA, RC);

            { map symmetric contexts }
            cont := FImageInfo^.classmap[cont];

            if (cont < 0) then
              begin
                Sign := -1;
                cont := -cont;
              end
            else
                Sign := +1;

            { decode a Rice code of a given context }
            RA := lossless_regular_mode_d(cont, Sign, Px);

          end;
        SL^[i] := RA;
        RC := rb;
        rb := Rd;
        inc(i);

      until not(i <= no);

    end
  else

    { ***********************************************/
      /* Do for all pixels in the row in 16-bit mode */
      /*********************************************** }
    begin

      RC := ENDIAN16(psl^[0]);
      rb := ENDIAN16(psl^[1]);
      RA := ENDIAN16(SL^[0]);

      i := 1;
      repeat

        Rd := ENDIAN16(psl^[i + 1]);

        { Quantize the gradient }

        { Following segment assumes that T3 <= LUTMAX16 }
        { This condition should have been checked when the
          lookup tables were built }
        Diff := Rd - rb;

        if (Diff < 0) then
          begin
            if (Diff > -LUTMAX16)
            then
                cont := FImageInfo^.vLUT[0][Diff + LUTMAX16]
            else
                cont := 7 * CREGIONS * CREGIONS
          end
        else
          begin
            if (Diff < LUTMAX16)
            then
                cont := FImageInfo^.vLUT[0][Diff + LUTMAX16]
            else
                cont := 8 * CREGIONS * CREGIONS;
          end;

        Diff := rb - RC;
        if (Diff < 0) then
          begin
            if (Diff > -LUTMAX16)
            then
                cont := cont + FImageInfo^.vLUT[1][Diff + LUTMAX16]
            else
                cont := cont + 7 * CREGIONS;
          end
        else
          begin
            if (Diff < LUTMAX16)
            then
                cont := cont + FImageInfo^.vLUT[1][Diff + LUTMAX16]
            else
                cont := cont + 8 * CREGIONS;
          end;

        Diff := RC - RA;
        if (Diff < 0) then
          begin
            if (Diff > -LUTMAX16)
            then
                cont := cont + FImageInfo^.vLUT[2][Diff + LUTMAX16]
            else
                cont := cont + 7;
          end
        else
          begin
            if (Diff < LUTMAX16)
            then
                cont := cont + FImageInfo^.vLUT[2][Diff + LUTMAX16]
            else
                cont := cont + 8;
          end;

        if (cont = 0) then
          begin

            { ********** RUN STATE ********** }

            { get length of the run }
            { arg is # of pixels left }
            n := FMelcode.process_run_dec(no - i + 1, COLOR);
            M := n;

            if (M > 0) then
              begin { run of nonzero length, otherwise
                  we go directly to the end-of-run
                  state }

                repeat

                  SL^[i] := ENDIAN16(RA);
                  inc(i);
                  dec(n);
                until not(n > 0);

                if (i > no) then
                  { end of line }
                  begin
                    Result := 0;
                    Exit;
                  end;

                { update context pixels }
                rb := ENDIAN16(psl^[i]);
                Rd := ENDIAN16(psl^[i + 1]);
              end;

            { Do end of run encoding for LOSSLESS images }
            if (RA = rb) then
                RA := lossless_end_of_run_d(RA, rb, 1)
            else
                RA := lossless_end_of_run_d(RA, rb, 0);

          end
        else
          begin
            { ********** REGULAR CONTEXT ********** }

            Px := predict(rb, RA, RC);

            { map symmetric contexts }
            cont := FImageInfo^.classmap[cont];

            if (cont < 0) then
              begin
                Sign := -1;
                cont := -cont;
              end
            else
                Sign := +1;

            { decode a Rice code of a given context }
            RA := lossless_regular_mode_d(cont, Sign, Px);

          end;
        SL^[i] := ENDIAN16(RA);

        // if  FDebugCounter<> ENDIAN16(Ra)
        // then raise Exception.Create('Error Message Pos : '+IntToStr(FDebugStream.Position-18)+' Pos Val: '+IntToStr(FDebugCounter)+'<> (Ra) i: '+IntTostr(ENDIAN16(Ra))+' i: '+IntToStr(i));

        RC := rb;
        rb := Rd;
        inc(i);

      until not(i <= no);
    end; { End "if 8/16 bit" }

  Result := 0;

end;

function TJLSLossless.lossless_undoscanline_pixel(psl, SL: ppixelarray;
  no: Int): Int;
var
  i, psfix, n_c, COLOR, enter_run, break_run, was_in_run, test_run: Int;
  Sign: Int;
  cont: Int;
  c_cont: array [0 .. MAX_COMPONENTS - 1] of Int;
  n, M: Int;
  RA, rb, RC, Rd, Px: Pixel;
  c_aa: array [0 .. MAX_COMPONENTS - 1] of Pixel;
  c_bb: array [0 .. MAX_COMPONENTS - 1] of Pixel;
  c_cc: array [0 .. MAX_COMPONENTS - 1] of Pixel;
  c_dd: array [0 .. MAX_COMPONENTS - 1] of Pixel;
  c_xx: array [0 .. MAX_COMPONENTS - 1] of Pixel;
  Diff: Int;
begin
  enter_run := 0;
  was_in_run := 0;
  psfix := 0;

  { **********************************************
    * Do for all pixels in the row in 8-bit mode *
    ********************************************** }
  if (FImageInfo^.bpp16 = False) then
    begin

      for n_c := 0 to pred(FImageInfo^.Components) do
        begin
          c_cc[n_c] := psl^[n_c];
          c_bb[n_c] := psl^[FImageInfo^.Components + n_c];
          c_aa[n_c] := SL^[n_c];
        end;

      i := FImageInfo^.Components;
      COLOR := -1;

      repeat

        if not IsTrue(was_in_run) then
            COLOR := (COLOR + 1) mod FImageInfo^.Components
        else
            COLOR := 0;

        if (COLOR = 0) then

          for n_c := 0 to pred(FImageInfo^.Components) do
            begin
              c_dd[n_c] := psl^[i + FImageInfo^.Components + n_c];

              { Quantize the gradient }
              c_cont[n_c] := FImageInfo^.vLUT[0][c_dd[n_c] - c_bb[n_c] + LUTMAX8] +
                FImageInfo^.vLUT[1][c_bb[n_c] - c_cc[n_c] + LUTMAX8] +
                FImageInfo^.vLUT[2][c_cc[n_c] - c_aa[n_c] + LUTMAX8];
            end;

        RA := c_aa[COLOR];
        rb := c_bb[COLOR];
        RC := c_cc[COLOR];
        Rd := c_dd[COLOR];
        cont := c_cont[COLOR];

        enter_run := 0;
        was_in_run := 0;
        test_run := 0;

        if (COLOR = 0) then
          begin
            test_run := 1;
            for n_c := 0 to pred(FImageInfo^.Components) do
              begin
                if (c_cont[n_c] <> 0) then
                  begin
                    test_run := 0;
                    Break;
                  end;
              end;
          end;

        if IsTrue(test_run) then
          begin
            { ********** RUN STATE ********* }
            enter_run := 0;
            was_in_run := 1;

            { get length of the run }
            { arg is # of pixels left }
            n := FMelcode.process_run_dec((no + FImageInfo^.Components - 1 - i + 1) div FImageInfo^.Components, 0);
            M := n;

            if (M > 0) then
              begin { run of nonzero length, otherwise
                  we go directly to the end-of-run
                  state }
                repeat
                  for n_c := 0 to pred(FImageInfo^.Components) do
                    begin
                      SL^[i] := c_aa[n_c];
                      inc(i);
                    end;
                  dec(n);
                until not(n > 0);

                if (i > no + FImageInfo^.Components - 1) then { end of line }
                  begin
                    Result := 0;
                    Exit;
                  end;
                { update context pixels }
                for n_c := 0 to pred(FImageInfo^.Components) do
                  begin
                    c_bb[n_c] := psl^[i + n_c];
                    c_dd[n_c] := psl^[i + FImageInfo^.Components + n_c];
                  end;

              end;

            { here we handle the "end-of-run" stat }

            for n_c := 0 to pred(FImageInfo^.Components) do
              begin
                { The end of run is processed for each component }
                RA := c_aa[n_c];
                rb := c_bb[n_c];
                c_xx[n_c] := lossless_end_of_run_d(RA, rb, 0);
                c_aa[n_c] := c_xx[n_c];
              end; { Components loop }

          end { Run state block }
        else
          begin

            { ******* REGULAR CONTEXT ******* }
            Px := predict(rb, RA, RC);

            cont := FImageInfo^.classmap[cont];

            if (cont < 0) then
              begin
                Sign := -1;
                cont := -cont;
              end
            else
                Sign := +1;

            { decode a Rice code of a given context }
            RA := lossless_regular_mode_d(cont, Sign, Px);
            c_aa[COLOR] := RA;
          end;

        if not IsTrue(was_in_run) then
          begin
            SL^[i] := RA;
            c_cc[COLOR] := rb;
            c_bb[COLOR] := Rd;
            inc(i);
          end
        else
          begin
            for n_c := 0 to pred(FImageInfo^.Components) do
              begin
                SL^[i + n_c] := c_aa[n_c];
                c_cc[n_c] := c_bb[n_c];
                c_bb[n_c] := c_dd[n_c];
              end;
            inc(i, FImageInfo^.Components);
          end;

      until not(i <= (no + FImageInfo^.Components - 1));

    end
  else
    begin
      { ***********************************************
        * Do for all pixels in the row in 16-bit mode *
        *********************************************** }

      for n_c := 0 to pred(FImageInfo^.Components) do
        begin
          c_cc[n_c] := ENDIAN16(psl^[n_c]);
          c_bb[n_c] := ENDIAN16(psl^[FImageInfo^.Components + n_c]);
          c_aa[n_c] := ENDIAN16(SL^[n_c]);
        end;

      i := FImageInfo^.Components;
      COLOR := -1;

      repeat

        if not IsTrue(was_in_run)
        then
            COLOR := (COLOR + 1) mod FImageInfo^.Components
        else
            COLOR := 0;

        if (COLOR = 0) then
          begin
            for n_c := 0 to pred(FImageInfo^.Components) do
              begin

                c_dd[n_c] := ENDIAN16(psl^[i + FImageInfo^.Components + n_c]);

                { Quantize the gradient }

                { Following segment assumes that T3 <= LUTMAX16 }
                { This condition should have been checked when the
                  lookup tables were built }
                Diff := c_dd[n_c] - c_bb[n_c];
                if (Diff < 0) then
                  begin
                    if (Diff > -LUTMAX16) then
                        c_cont[n_c] := FImageInfo^.vLUT[0][Diff + LUTMAX16]
                    else
                        c_cont[n_c] := 7 * CREGIONS * CREGIONS;
                  end
                else
                  begin
                    if (Diff < LUTMAX16) then
                        c_cont[n_c] := FImageInfo^.vLUT[0][Diff + LUTMAX16]
                    else
                        c_cont[n_c] := 8 * CREGIONS * CREGIONS;
                  end;

                Diff := c_bb[n_c] - c_cc[n_c];
                if (Diff < 0) then
                  begin
                    if (Diff > -LUTMAX16) then
                        c_cont[n_c] := c_cont[n_c] + FImageInfo^.vLUT[1][Diff + LUTMAX16]
                    else
                        c_cont[n_c] := c_cont[n_c] + 7 * CREGIONS;
                  end
                else
                  begin
                    if (Diff < LUTMAX16) then
                        c_cont[n_c] := c_cont[n_c] + FImageInfo^.vLUT[1][Diff + LUTMAX16]
                    else
                        c_cont[n_c] := c_cont[n_c] + 8 * CREGIONS;
                  end;

                Diff := c_cc[n_c] - c_aa[n_c];
                if (Diff < 0) then
                  begin
                    if (Diff > -LUTMAX16) then
                        c_cont[n_c] := c_cont[n_c] + FImageInfo^.vLUT[2][Diff + LUTMAX16]
                    else
                        c_cont[n_c] := c_cont[n_c] + 7;
                  end
                else
                  begin
                    if (Diff < LUTMAX16) then
                        c_cont[n_c] := c_cont[n_c] + FImageInfo^.vLUT[2][Diff + LUTMAX16]
                    else
                        c_cont[n_c] := c_cont[n_c] + 8;
                  end;

              end;
          end;

        RA := c_aa[COLOR];
        rb := c_bb[COLOR];
        RC := c_cc[COLOR];
        Rd := c_dd[COLOR];
        cont := c_cont[COLOR];

        enter_run := 0;
        was_in_run := 0;
        test_run := 0;

        if (COLOR = 0) then
          begin
            test_run := 1;
            for n_c := 0 to pred(FImageInfo^.Components) do
              if (c_cont[n_c] <> 0) then
                begin
                  test_run := 0;
                  Break;
                end;
          end;

        if IsTrue(test_run) then
          begin

            { ********* RUN STATE ********* }
            enter_run := 1;
            was_in_run := 1;

            { get length of the run }
            { arg is # of pixels left }
            n := FMelcode.process_run_dec((no + FImageInfo^.Components - 1 - i + 1) div FImageInfo^.Components, 0);
            M := n;
            if (M > 0) then
              begin { run of nonzero length, otherwise
                  we go directly to the end-of-run
                  state }

                repeat

                  SL^[i] := ENDIAN16(c_aa[n_c]);
                  inc(i);
                  dec(n);
                until not(n > 0);

                if (i > no) then
                  { end of line }
                  begin
                    Result := 0;
                    Exit;
                  end;

                { update context pixels }
                c_bb[n_c] := ENDIAN16(psl^[i + n_c]);
                c_dd[n_c] := ENDIAN16(psl^[i + FImageInfo^.Components + n_c]);
              end;

            { here we handle the "end-of-run" state }
            for n_c := 0 to pred(FImageInfo^.Components) do
              begin
                { The end of run is processed for each component }
                RA := c_aa[n_c];
                rb := c_bb[n_c];
                c_xx[n_c] := lossless_end_of_run_d(RA, rb, 0);
                c_aa[n_c] := c_xx[n_c];
              end; { Components loop }

          end { Run state block }
        else
          begin

            { ******** REGULAR CONTEXT ******** }

            Px := predict(rb, RA, RC);

            cont := FImageInfo^.classmap[cont];

            if (cont < 0) then
              begin
                Sign := -1;
                cont := -cont;
              end
            else
                Sign := +1;

            { decode a Rice code of a given context }
            RA := lossless_regular_mode_d(cont, Sign, Px);
            c_aa[COLOR] := RA;

          end;

        if not IsTrue(was_in_run) then
          begin
            SL^[i] := ENDIAN16(RA);
            c_cc[COLOR] := rb;
            c_bb[COLOR] := Rd;
            inc(i);
          end
        else begin
            for n_c := 0 to pred(FImageInfo^.Components) do
              begin
                SL^[i + n_c] := ENDIAN16(c_aa[n_c]);
                c_cc[n_c] := c_bb[n_c];
                c_bb[n_c] := c_dd[n_c];
              end;
            i := i + FImageInfo^.Components;
          end;

      until not(i <= (no + FImageInfo^.Components - 1));

    end; { ends "if 8/16 bit }

  Result := 0;
end;

{ Do end of run encoding for LOSSLESS images }
procedure TJLSLossless.lossless_end_of_run_e(RA, rb, ix: Pixel; RItype: Int);
var
  Errval,
    MErrval,
    q,
    absErrval,
    oldmap,
    k,
    at,
    unary: Int;
  Nt: Int;
begin
  q := EOR_0 + RItype;
  Nt := FImageInfo^.n[q];
  at := FImageInfo^.a[q];

  Errval := ix - rb;
  if IsTrue(RItype) then
      at := at + shr_c(Nt, 1)
  else begin
      if (rb < RA) then
          Errval := -Errval;
    end;

  { Estimate k }
  k := 0;
  while (Nt < at) do
    begin
      Nt := Nt shl 1;
      inc(k);
    end;

  if (Errval < 0) then
      Errval := Errval + FImageInfo^.alpha;
  if (Errval >= FImageInfo^.ceil_half_alpha) then
      Errval := Errval - FImageInfo^.alpha;

  if ((k = 0) and IsTrue(Errval) and ((FImageInfo^.b[q] shl 1) < Nt)) then
      oldmap := 1
  else
      oldmap := 0;

  { Note: the Boolean variable 'oldmap' is not
    identical to the variable 'map' in the
    JPEG-LS draft. We have
    oldmap = (Errval<0) ? (1-map) : map;
  }

  { Error mapping for run-interrupted sample (Figure A.22) }
  if (Errval < 0) then
    begin
      MErrval := -(Errval shl 1) - 1 - RItype + oldmap;
      inc(FImageInfo^.b[q]);
    end
  else
      MErrval := (Errval shl 1) - RItype - oldmap;

  absErrval := shr_c((MErrval + 1 - RItype), 1);

  { Update variables for run-interruped sample (Figure A.23) }
  FImageInfo^.a[q] := FImageInfo^.a[q] + absErrval;

  if (FImageInfo^.n[q] = FImageInfo^.Reset) then
    begin
      FImageInfo^.n[q] := shr_c(FImageInfo^.n[q], 1);
      FImageInfo^.a[q] := shr_c(FImageInfo^.a[q], 1);
      FImageInfo^.b[q] := shr_c(FImageInfo^.b[q], 1);
    end;

  inc(FImageInfo^.n[q]); { for next pixel }

  { Do the actual Golomb encoding: }
  eor_limit := FImageInfo^.Limit - FImageInfo^.limit_reduce;
  unary := shr_c(MErrval, k);
  if (unary < eor_limit) then
    begin
      FBitIO.put_zeros(unary);
      FBitIO.putbits((1 shl k) + (MErrval and ((1 shl k) - 1)), k + 1);
    end
  else begin
      FBitIO.put_zeros(eor_limit);
      FBitIO.putbits((1 shl FImageInfo^.qbpp) + MErrval - 1, FImageInfo^.qbpp + 1);
    end;
end;

{ Do Golomb statistics and ENCODING for LOSS-LESS images }
procedure TJLSLossless.lossless_regular_mode_e(q, Sign, Px: Int; xp: ppixel);
var
  at, Nt, nst, Bt, absErrval, Errval, MErrval, ix: Int;
  unary: Int;
  Temp: Int;
  k: Byte;

begin
  ix := xp^; { current pixel }

  Nt := FImageInfo^.n[q];
  at := FImageInfo^.a[q];

  { Prediction correction (A.4.2), compute prediction error (A.4.3)
    , and error quantization (A.4.4) }
  Px := Px + (Sign) * FImageInfo^.c[q];
  Clip(Px, FImageInfo^.alpha);
  Errval := Sign * (ix - Px);

  { Modulo reduction of predication error (A.4.5) }
  if (Errval < 0) then
      Errval := Errval + FImageInfo^.alpha; { Errval is now in [0.. alpha-1] }

  { Estimate k - Golomb coding variable computation (A.5.1) }

  nst := Nt;
  k := 0;
  while (nst < at) do begin nst := nst shl 1; inc(k);
    end;

  { Do Rice mapping and compute magnitude of Errval }
  Bt := FImageInfo^.b[q];

  { Error Mapping (A.5.2) }
  if (k = 0) and ((Bt shl 1) <= -Nt)
  then
      Temp := 1
  else
      Temp := 0;

  if (Errval >= FImageInfo^.ceil_half_alpha) then
    begin
      Errval := Errval - FImageInfo^.alpha;
      absErrval := -Errval;
      MErrval := (absErrval shl 1) - 1 - Temp;
    end
  else begin
      absErrval := Errval;
      MErrval := (Errval shl 1) + Temp;
    end;

  { update bias stats (after correction of the difference) (A.6.1) }
  Bt := Bt + Errval;
  FImageInfo^.b[q] := Bt;

  { update Golomb stats }
  FImageInfo^.a[q] := FImageInfo^.a[q] + absErrval;

  { check for reset }
  if (Nt = FImageInfo^.Reset) then
    begin
      { reset for Golomb and bias cancelation at the same time }
      Nt := shr_c(Nt, 1);
      FImageInfo^.n[q] := Nt;
      FImageInfo^.a[q] := shr_c(FImageInfo^.a[q], 1);
      Bt := shr_c(Bt, 1);
      FImageInfo^.b[q] := Bt;
    end;

  inc(Nt);
  FImageInfo^.n[q] := Nt;

  { Do bias estimation for NEXT pixel }
  { Bias cancelation tries to put error in (-1,0] (A.6.2) }
  if (Bt <= -Nt) then
    begin
      if (FImageInfo^.c[q] > MIN_C) then
          dec(FImageInfo^.c[q]);
      FImageInfo^.b[q] := FImageInfo^.b[q] + Nt;
      if (FImageInfo^.b[q] <= -Nt) then
          FImageInfo^.b[q] := -Nt + 1;

    end
  else if (Bt > 0) then
    begin

      if (FImageInfo^.c[q] < MAX_C) then
          inc(FImageInfo^.c[q]);
      FImageInfo^.b[q] := FImageInfo^.b[q] - Nt;
      if (FImageInfo^.b[q] > 0) then
          FImageInfo^.b[q] := 0;
    end;

  { Actually output the code: Mapped Error Encoding (Appendix G) }
  unary := shr_c(MErrval, k);
  if (unary < FImageInfo^.Limit) then
    begin
      FBitIO.put_zeros(unary);
      FBitIO.putbits((1 shl k) + (MErrval and ((1 shl k) - 1)), k + 1);
    end
  else begin
      FBitIO.put_zeros(FImageInfo^.Limit);
      FBitIO.putbits((1 shl FImageInfo^.qbpp) + MErrval - 1, FImageInfo^.qbpp + 1);
    end;
end;

{ For line and plane interleaved mode in LOSS-LESS mode }
constructor TJLSLossless.Create(ABitIO: TJLSBitIO; AMelcode: TJLSMelcode; AImageInfo: PImageInfo);
begin
  FBitIO := ABitIO;
  FMelcode := AMelcode;
  FImageInfo := AImageInfo;
end;

procedure TJLSLossless.lossless_doscanline(psl: ppixelarray; { previous scanline }
  SL: ppixelarray;                                           { current scanline }
  no, COLOR: Int);                                           { number of values in it }
var
  i: Int;
  RA, rb, RC, Rd, { context pixels }
  ix,             { current pixel }
  Px: Pixel;      { predicted current pixel }

  Sign: Int; { sign of current context }
  cont: Int; { context }

  RUNcnt: Int;
  Diff: Int;
begin
  { *** watch it! actual pixels in the scan line are numbered 1 to no .
    pixels with indices < 1 or > no are dummy "border" pixels }

  i := 1; { pixel indices in a scan line go from 1 to no }

  { **********************************************/
    /* Do for all pixels in the row in 8-bit mode */
    /********************************************** }
  if (FImageInfo^.bpp16 = False) then
    begin

      RC := psl^[0];
      rb := psl^[1];
      RA := SL^[0];

      { For 8-bit Image }

      repeat

        ix := SL^[i];
        Rd := psl^[i + 1];

        { Context determination }

        { Quantize the gradient }
        { partial context number: if (b-e) is used then its
          contribution is added after determination of the run state.
          Also, sign flipping, if any, occurs after run
          state determination }

        cont := FImageInfo^.vLUT[0][Rd - rb + LUTMAX8] +
          FImageInfo^.vLUT[1][rb - RC + LUTMAX8] +
          FImageInfo^.vLUT[2][RC - RA + LUTMAX8];

        if (cont = 0) then
          begin
            { *************** RUN STATE *************************** }

            RUNcnt := 0;

            if (ix = RA) then
              begin
                while (True) do
                  begin

                    inc(RUNcnt);
                    inc(i);
                    if (i > no) then
                      begin
                        { Run-lenght coding when reach end of line (A.7.1.2) }
                        FMelcode.process_run_enc(RUNcnt, EOLINE, COLOR);
                        Exit; { end of line }
                      end;

                    ix := SL^[i];

                    if (ix <> RA) then { Run is broken }
                      begin
                        Rd := psl^[i + 1];
                        rb := psl^[i];
                        Break; { out of while loop }
                      end;
                    { Run continues }
                  end;
              end;

            { we only get here if the run is broken by
              a non-matching symbol }

            { Run-lenght coding when end of line not reached (A.7.1.2) }
            FMelcode.process_run_enc(RUNcnt, NOEOLINE, COLOR);

            { This is the END_OF_RUN state }
            if (RA = rb)
            then
                lossless_end_of_run_e(RA, rb, ix, 1)
            else
                lossless_end_of_run_e(RA, rb, ix, 0);

          end
        else
          begin

            { *************** REGULAR CONTEXT ******************* }

            Px := predict(rb, RA, RC);

            { do symmetric context merging }
            cont := FImageInfo^.classmap[cont];

            if (cont < 0) then
              begin
                Sign := -1;
                cont := -cont;
              end
            else
                Sign := +1;

            { output a rice code }
            lossless_regular_mode_e(cont, Sign, Px, @ix);
          end;

        { context for next pixel: }
        SL^[i] := ix;

        RA := ix;
        RC := rb;
        rb := Rd;
        inc(i);
      until not(i <= no);

    end
  else
    begin

      { *********************************************** }
      { * Do for all pixels in the row in 16-bit mode * }
      { *********************************************** }

      RC := ENDIAN16(psl^[0]);
      rb := ENDIAN16(psl^[1]);
      RA := ENDIAN16(SL^[0]);

      { For 16-bit Image }

      repeat

        ix := ENDIAN16(SL^[i]);
        Rd := ENDIAN16(psl^[i + 1]);

        { Context determination }

        { Quantize the gradient }
        { partial context number: if (b-e) is used then its
          contribution is added after determination of the run state.
          Also, sign flipping, if any, occurs after run
          state determination }

        { Following segment assumes that T3 <= LUTMAX16 }
        { This condition should have been checked when the
          lookup tables were built }
        Diff := Rd - rb;

        if (Diff < 0) then
          begin
            if (Diff > -LUTMAX16)
            then
                cont := FImageInfo^.vLUT[0][Diff + LUTMAX16]
            else
                cont := 7 * CREGIONS * CREGIONS;
          end
        else
          begin
            if (Diff < LUTMAX16)
            then
                cont := FImageInfo^.vLUT[0][Diff + LUTMAX16]
            else
                cont := 8 * CREGIONS * CREGIONS;
          end;

        Diff := rb - RC;

        if (Diff < 0) then
          begin
            if (Diff > -LUTMAX16)
            then
                cont := cont + FImageInfo^.vLUT[1][Diff + LUTMAX16]
            else
                cont := cont + 7 * CREGIONS;
          end
        else
          begin
            if (Diff < LUTMAX16)
            then
                cont := cont + FImageInfo^.vLUT[1][Diff + LUTMAX16]
            else
                cont := cont + 8 * CREGIONS;
          end;

        Diff := RC - RA;

        if (Diff < 0) then
          begin
            if (Diff > -LUTMAX16)
            then
                cont := cont + FImageInfo^.vLUT[2][Diff + LUTMAX16]
            else
                cont := cont + 7;
          end
        else
          begin
            if (Diff < LUTMAX16)
            then
                cont := cont + FImageInfo^.vLUT[2][Diff + LUTMAX16]
            else
                cont := cont + 8;
          end;

        if (cont = 0) then { Run state? }
          begin
            { *************** RUN STATE *************************** }

            RUNcnt := 0;

            if (ix = RA) then
              begin
                while True do
                  begin

                    inc(RUNcnt);
                    inc(i);
                    if (i > no) then
                      begin
                        { Run-lenght coding when reach end of line (A.7.1.2) }
                        FMelcode.process_run_enc(RUNcnt, EOLINE, COLOR);
                        Exit; { end of line }
                      end;

                    ix := ENDIAN16(SL^[i]);

                    if (ix <> RA) then { Run is broken }
                      begin
                        Rd := ENDIAN16(psl^[i + 1]);
                        rb := ENDIAN16(psl^[i]);
                        Break; { out of while loop }
                      end;
                    { Run continues }
                  end;
              end;

            { we only get here if the run is broken by
              a non-matching symbol }

            { Run-lenght coding when end of line not reached (A.7.1.2) }
            FMelcode.process_run_enc(RUNcnt, NOEOLINE, COLOR);

            { This is the END_OF_RUN state }
            if (RA = rb)
            then
                lossless_end_of_run_e(RA, rb, ix, 1)
            else
                lossless_end_of_run_e(RA, rb, ix, 0);

          end
        else begin

            { *************** REGULAR CONTEXT ******************* }

            Px := predict(rb, RA, RC);

            { do symmetric context merging }

            cont := FImageInfo^.classmap[cont];

            if (cont < 0) then
              begin
                Sign := -1;
                cont := -cont;
              end
            else
                Sign := +1;

            { output a rice code }
            lossless_regular_mode_e(cont, Sign, Px, @ix);
          end;

        { context for next pixel: }
        SL^[i] := ENDIAN16(ix);
        RA := ix;
        RC := rb;
        rb := Rd;
        inc(i);
      until not(i <= no);
    end;

end;

procedure TJLSLossless.lossless_doscanline_pixel(psl, SL: ppixelarray;
  no: Int);
var
  i, n_c, enter_run, break_run, was_in_run, test_run: Int;
  COLOR: Int; { Index to the component, 0..COMPONENTS-1 }
  c_aa: array [0 .. MAX_COMPONENTS - 1] of Pixel;
  c_bb: array [0 .. MAX_COMPONENTS - 1] of Pixel;
  c_cc: array [0 .. MAX_COMPONENTS - 1] of Pixel;
  c_dd: array [0 .. MAX_COMPONENTS - 1] of Pixel;
  c_xx: array [0 .. MAX_COMPONENTS - 1] of Pixel;
  RA, rb, RC, Rd, { context pixels }
  ix,             { current pixel }
  Px: Pixel;      { predicted current pixel }
  Sign: Int;      { sign of current context }
  cont: Int;
  c_cont: array [0 .. MAX_COMPONENTS - 1] of Int; { context }
  RUNcnt: Int;
  Diff: Int;
begin

  enter_run := 0;
  was_in_run := 0;

  if (FImageInfo^.bpp16 = False) then
    begin
      { **********************************************
        * Do for all pixels in the row in 8-bit mode *
        ********************************************** }

      for n_c := 0 to pred(FImageInfo^.Components) do
        begin
          c_cc[n_c] := ENDIAN8(psl^[n_c]);
          c_bb[n_c] := ENDIAN8(psl^[FImageInfo^.Components + n_c]);
          c_aa[n_c] := ENDIAN8(SL^[n_c]);
        end;

      i := FImageInfo^.Components; { pixel indices in a scan line go from COMPONENTS to no }
      COLOR := -1;

      repeat

        if not IsTrue(was_in_run) then
            COLOR := (COLOR + 1) mod FImageInfo^.Components
        else
            COLOR := 0;

        ix := ENDIAN8(SL^[i]);

        for n_c := 0 to pred(FImageInfo^.Components) do
            c_xx[n_c] := ENDIAN8(SL^[i + n_c]);

        if (COLOR = 0) then
          for n_c := 0 to pred(FImageInfo^.Components) do
            begin
              c_dd[n_c] := ENDIAN8(psl^[i + FImageInfo^.Components + n_c]);

              { Context determination }

              { Quantize the gradient }
              { partial context number: if (b-e) is used
                then its contribution is added after
                determination of the run state.
                Also, sign flipping, if any, occurs after run
                state determination }

              c_cont[n_c] := FImageInfo^.vLUT[0][c_dd[n_c] - c_bb[n_c] + LUTMAX8] +
                FImageInfo^.vLUT[1][c_bb[n_c] - c_cc[n_c] + LUTMAX8] +
                FImageInfo^.vLUT[2][c_cc[n_c] - c_aa[n_c] + LUTMAX8];
            end;

        RA := c_aa[COLOR];
        rb := c_bb[COLOR];
        RC := c_cc[COLOR];
        Rd := c_dd[COLOR];
        cont := c_cont[COLOR];

        enter_run := 0;
        was_in_run := 0;
        test_run := 0;

        if (COLOR = 0) then
          begin
            test_run := 1;
            for n_c := 0 to pred(FImageInfo^.Components) do
              if (c_cont[n_c] <> 0) then
                begin
                  test_run := 0;
                  Break;
                end;
          end;

        { Run state? }
        if IsTrue(test_run) then
          begin
            { *************** RUN STATE *************************** }

            enter_run := 1;
            was_in_run := 1;
            for n_c := 0 to pred(FImageInfo^.Components) do
              if (ENDIAN8(SL^[i + n_c]) <> c_bb[n_c]) then
                  enter_run := 0;
            RUNcnt := 0;
            if IsTrue(enter_run) then
              begin
                while True do
                  begin
                    inc(RUNcnt);
                    i := i + FImageInfo^.Components;
                    if ((i) > (no + FImageInfo^.Components - 1)) then
                      begin
                        FMelcode.process_run_enc(RUNcnt, EOLINE, 0);
                        Exit; { end of line }
                      end;

                    for n_c := 0 to pred(FImageInfo^.Components) do
                        c_xx[n_c] := ENDIAN8(SL^[i + n_c]);

                    break_run := 0;

                    for n_c := 0 to pred(FImageInfo^.Components) do
                      if (c_xx[n_c] <> c_aa[n_c]) then
                          break_run := 1;

                    if IsTrue(break_run) then { Run is broken }
                      begin
                        for n_c := 0 to pred(FImageInfo^.Components) do
                          begin
                            c_dd[n_c] := ENDIAN8(psl^[i + FImageInfo^.Components + n_c]);
                            c_bb[n_c] := ENDIAN8(psl^[i + n_c]);
                          end;
                        Break; { out of while loop }
                      end;
                    { Run continues }
                  end;
              end;

            { we only get here if the run is broken by
              a non-matching symbol }

            FMelcode.process_run_enc(RUNcnt, NOEOLINE, 0);

            { This is the END_OF_RUN state }
            for n_c := 0 to pred(FImageInfo^.Components) do
              begin
                { The end of run is done for each component }
                ix := c_xx[n_c];
                RA := c_aa[n_c];
                rb := c_bb[n_c];

                lossless_end_of_run_e(RA, rb, ix, 0);

              end; { loop for components }

          end { Run state block }
        else
          begin

            { *************** REGULAR CONTEXT ******************* }
            Px := predict(rb, RA, RC);

            { map symmetric contexts }
            cont := FImageInfo^.classmap[cont];

            if (cont < 0) then
              begin
                Sign := -1;
                cont := -cont;
              end
            else
                Sign := +1;

            { output a rice code }

            lossless_regular_mode_e(cont, Sign, Px, @ix);
          end;

        { context for next pixel: }
        if not IsTrue(was_in_run) then
          begin
            c_aa[COLOR] := ix;
            c_cc[COLOR] := rb;
            c_bb[COLOR] := Rd;
            inc(i);
          end
        else
          begin
            for n_c := 0 to pred(FImageInfo^.Components) do
              begin
                c_aa[n_c] := c_xx[n_c];
                c_cc[n_c] := c_bb[n_c];
                c_bb[n_c] := c_dd[n_c];
              end;
            i := i + FImageInfo^.Components;
          end;

      until not(i <= (no + FImageInfo^.Components - 1));

    end
  else
    begin

      { **********************************************
        * Do for all pixels in the row in 16-bit mode*
        ********************************************** }

      for n_c := 0 to pred(FImageInfo^.Components) do
        begin
          c_cc[n_c] := ENDIAN16(psl^[n_c]);
          c_bb[n_c] := ENDIAN16(psl^[FImageInfo^.Components + n_c]);
          c_aa[n_c] := ENDIAN16(SL^[n_c]);
        end;

      i := FImageInfo^.Components; { pixel indices in a scan line go from COMPONENTS to no }
      COLOR := -1;

      repeat

        if not IsTrue(was_in_run) then
            COLOR := (COLOR + 1) mod FImageInfo^.Components
        else
            COLOR := 0;

        ix := ENDIAN16(SL^[i]);

        for n_c := 0 to pred(FImageInfo^.Components) do
            c_xx[n_c] := ENDIAN16(SL^[i + n_c]);

        if (COLOR = 0) then
          for n_c := 0 to pred(FImageInfo^.Components) do
            begin
              c_dd[n_c] := ENDIAN16(psl^[i + FImageInfo^.Components + n_c]);

              { Context determination }

              { Quantize the gradient }
              { partial context number: if (b-e) is used
                then its contribution is added after
                determination of the run state.
                Also, sign flipping, if any, occurs after run
                state determination }

              { Following segment assumes that Sc <= LUTMAX16 }
              { This condition should have been checked when the
                lookup tables were built }
              Diff := c_dd[n_c] - c_bb[n_c];
              if (Diff < 0) then
                begin
                  if (Diff > -LUTMAX16) then
                      c_cont[n_c] := FImageInfo^.vLUT[0][Diff + LUTMAX16]
                  else
                      c_cont[n_c] := 7 * CREGIONS * CREGIONS;
                end
              else
                begin
                  if (Diff < LUTMAX16) then
                      c_cont[n_c] := FImageInfo^.vLUT[0][Diff + LUTMAX16]
                  else
                      c_cont[n_c] := 8 * CREGIONS * CREGIONS;
                end;

              Diff := c_bb[n_c] - c_cc[n_c];
              if (Diff < 0) then
                begin
                  if (Diff > -LUTMAX16) then
                      c_cont[n_c] := c_cont[n_c] + FImageInfo^.vLUT[1][Diff + LUTMAX16]
                  else
                      c_cont[n_c] := c_cont[n_c] + 7 * CREGIONS;
                end
              else
                begin
                  if (Diff < LUTMAX16) then
                      c_cont[n_c] := c_cont[n_c] + FImageInfo^.vLUT[1][Diff + LUTMAX16]
                  else
                      c_cont[n_c] := c_cont[n_c] + 8 * CREGIONS;
                end;

              Diff := c_cc[n_c] - c_aa[n_c];
              if (Diff < 0) then
                begin
                  if (Diff > -LUTMAX16) then
                      c_cont[n_c] := c_cont[n_c] + FImageInfo^.vLUT[2][Diff + LUTMAX16]
                  else
                      c_cont[n_c] := c_cont[n_c] + 7;
                end
              else
                begin
                  if (Diff < LUTMAX16) then
                      c_cont[n_c] := c_cont[n_c] + FImageInfo^.vLUT[2][Diff + LUTMAX16]
                  else
                      c_cont[n_c] := c_cont[n_c] + 8;
                end;
            end;

        RA := c_aa[COLOR];
        rb := c_bb[COLOR];
        RC := c_cc[COLOR];
        Rd := c_dd[COLOR];
        cont := c_cont[COLOR];

        enter_run := 0;
        was_in_run := 0;
        test_run := 0;

        if (COLOR = 0) then
          begin
            test_run := 1;
            for n_c := 0 to pred(FImageInfo^.Components) do
              if (c_cont[n_c] <> 0) then
                begin
                  test_run := 0;
                  Break;
                end;
          end;

        { Run state? }
        if IsTrue(test_run) then
          begin
            { *************** RUN STATE *************************** }

            enter_run := 1;
            was_in_run := 1;
            for n_c := 0 to pred(FImageInfo^.Components) do
              if (ENDIAN16(SL^[i + n_c]) <> c_bb[n_c]) then
                  enter_run := 0;
            RUNcnt := 0;
            if IsTrue(enter_run) then
              begin
                while True do
                  begin
                    inc(RUNcnt);
                    i := i + FImageInfo^.Components;
                    if ((i) > (no + FImageInfo^.Components - 1)) then
                      begin
                        FMelcode.process_run_enc(RUNcnt, EOLINE, 0);
                        Exit; { end of line }
                      end;

                    for n_c := 0 to pred(FImageInfo^.Components) do
                        c_xx[n_c] := ENDIAN16(SL^[i + n_c]);

                    break_run := 0;

                    for n_c := 0 to pred(FImageInfo^.Components) do
                      if (c_xx[n_c] <> c_aa[n_c]) then
                          break_run := 1;

                    if IsTrue(break_run) then { Run is broken }
                      begin
                        for n_c := 0 to pred(FImageInfo^.Components) do
                          begin
                            c_dd[n_c] := ENDIAN16(psl^[i + FImageInfo^.Components + n_c]);
                            c_bb[n_c] := ENDIAN16(psl^[i + n_c]);
                          end;
                        Break; { out of while loop }
                      end;
                    { Run continues }
                  end;
              end;

            { we only get here if the run is broken by
              a non-matching symbol }
            FMelcode.process_run_enc(RUNcnt, NOEOLINE, 0);

            { This is the END_OF_RUN state }
            for n_c := 0 to pred(FImageInfo^.Components) do
              begin
                { The end of run is done for each component }
                ix := c_xx[n_c];
                RA := c_aa[n_c];
                rb := c_bb[n_c];

                lossless_end_of_run_e(RA, rb, ix, 0);

              end; { loop for components }

          end { Run state block }
        else
          begin
            { *************** REGULAR CONTEXT ******************* }
            Px := predict(rb, RA, RC);
            cont := FImageInfo^.classmap[cont];

            if (cont < 0) then
              begin
                Sign := -1;
                cont := -cont;
              end
            else
                Sign := +1;

            { output a rice code }
            lossless_regular_mode_e(cont, Sign, Px, @ix);
          end;

        { context for next pixel: }
        if not IsTrue(was_in_run) then
          begin
            c_aa[COLOR] := ix;
            c_cc[COLOR] := rb;
            c_bb[COLOR] := Rd;
            inc(i);
          end
        else
          begin
            for n_c := 0 to pred(FImageInfo^.Components) do
              begin
                c_aa[n_c] := c_xx[n_c];
                c_cc[n_c] := c_bb[n_c];
                c_bb[n_c] := c_dd[n_c];
              end;
            i := i + FImageInfo^.Components;
          end;

      until not(i <= (no + FImageInfo^.Components - 1));

    end; { ends "if" for 8 or 16 bit }

end;

end. 
 
 
 
