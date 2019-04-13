{ ****************************************************************************** }
{ * JPEG-LS Codec https://github.com/zekiguven/pascal_jls                      * }
{ * fixed by QQ 600585@qq.com                                                  * }
{ ****************************************************************************** }
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
{
  JPEG-LS Codec
  This code is based on http://www.stat.columbia.edu/~jakulin/jpeg-ls/mirror.htm
  Converted from C to Pascal. 2017

  https://github.com/zekiguven/pascal_jls

  author : Zeki Guven
}
unit JLSMelcode;

{$INCLUDE zDefine.inc}

interface

uses
  JLSGlobal, JLSBitIO;

const
  MELCSTATES                                    = 32; { number of melcode states }

  j: array [0 .. MELCSTATES - 1] of Byte = (
    0, 0, 0, 0,
    1, 1, 1, 1,
    2, 2, 2, 2,
    3, 3, 3, 3,
    4, 4,
    5, 5,
    6, 6,
    7, 7,
    8,
    9,
    10,
    11,
    12,
    13,
    14,
    15
    );

type
  TJLSMelcode = class
  private
    FBitIO: TJLSBitIO;
    FImageInfo: PImageInfo;

    melcstate: array [0 .. MAX_COMPONENTS - 1] of Int; { index to the state array }

    melclen: array [0 .. MAX_COMPONENTS - 1] of Int;
    { contents of the state array location
      indexed by melcstate: the "expected"
      run length is 2^melclen, shorter runs are
      encoded by a 1 followed by the run length
      in binary representation, wit a fixed length
      of melclen bits }

    melcorder: array [0 .. MAX_COMPONENTS - 1] of ulong; { 2^ melclen }
  public
    constructor Create(ABitIO: TJLSBitIO; AImageInfo: PImageInfo);
    procedure init_process_run;
    function process_run_dec(lineleft: Int; COLOR: Int): Int;
    procedure process_run_enc(runlen: Int; an_eoline: Int; COLOR: Int);
    procedure close_process_run;
  end;

implementation

constructor TJLSMelcode.Create(ABitIO: TJLSBitIO; AImageInfo: PImageInfo);
begin
  FBitIO := ABitIO;
  FImageInfo := AImageInfo;
end;

procedure TJLSMelcode.init_process_run;
var
  n_c: Int;
begin

  for n_c := 0 to pred(FImageInfo^.Components) do
    begin
      melcstate[n_c] := 0;
      melclen[n_c] := j[0];
      melcorder[n_c] := 1 shl melclen[n_c];
    end;
end;

{ decoding routine: reads bits from the input and returns a run length. }
{ argument is the number of pixels left to  end-of-line (bound on run length) }
function TJLSMelcode.process_run_dec(lineleft: Int; COLOR: Int): Int;
var
  runlen: Int;
  Temp, hits: Int;
begin
  runlen := 0;

  while True do
    begin
      Temp := FBitIO.zeroLUT[Byte((not shr_c(FBitIO.reg, 24)))]; { number of leading ones in the
        input stream, up to 8 }
      for hits := 1 to Temp do
        begin
          runlen := runlen + melcorder[COLOR];
          if (runlen >= lineleft) then
            begin { reached end-of-line }
              if (runlen = lineleft) and (melcstate[COLOR] < MELCSTATES) then
                begin
                  inc(melcstate[COLOR]);
                  melclen[COLOR] := j[melcstate[COLOR]];
                  melcorder[COLOR] := (1 shl melclen[COLOR]);
                end;
              FBitIO.fillbuffer(hits); { actual # of 1's consumed }
              Result := lineleft;
              Exit;
            end;
          if (melcstate[COLOR] < MELCSTATES) then
            begin
              inc(melcstate[COLOR]);
              melclen[COLOR] := j[melcstate[COLOR]];
              melcorder[COLOR] := (1 shl melclen[COLOR]);
            end;
        end;
      if (Temp <> 8) then
        begin
          FBitIO.fillbuffer(Temp + 1); { consume the leading 0 of the remainder encoding }
          Break;
        end;
      FBitIO.fillbuffer(8);
    end;

  { read the length of the remainder }
  if IsTrue(melclen[COLOR]) then
    begin
      Temp := shr_c(FBitIO.reg, 32 - (melclen[COLOR]));
      FBitIO.fillbuffer(melclen[COLOR]);
      // GETBITS(temp, melclen[color]);  /*** GETBITS is a macro, not a function */
      runlen := runlen + Temp;
    end;
  FImageInfo^.limit_reduce := melclen[COLOR] + 1;

  { adjust melcoder parameters }
  if IsTrue(melcstate[COLOR]) then
    begin
      dec(melcstate[COLOR]);
      melclen[COLOR] := j[melcstate[COLOR]];
      melcorder[COLOR] := (1 shl melclen[COLOR]);
    end;

  Result := runlen;
end;

procedure TJLSMelcode.process_run_enc(runlen: Int; an_eoline: Int; COLOR: Int);
var
  hits: Int;
begin
  hits := 0;

  while (runlen >= melcorder[COLOR]) do
    begin
      inc(hits);
      runlen := runlen - melcorder[COLOR];
      if (melcstate[COLOR] < MELCSTATES) then
        begin
          inc(melcstate[COLOR]);
          melclen[COLOR] := j[melcstate[COLOR]];
          melcorder[COLOR] := (1 shl melclen[COLOR]);
        end;
    end;

  { send the required number of "hit" bits (one per occurrence
    of a run of length melcorder). This number is never too big:
    after 31 such "hit" bits, each "hit" would represent a run of 32K
    pixels.
  }
  FBitIO.put_ones(hits);

  if (an_eoline = EOLINE) then
    begin
      { when the run is broken by end-of-line, if there is
        a non-null remainder, send it as if it were
        a max length run }
      if IsTrue(runlen) then
          FBitIO.put_ones(1);
      Exit;
    end;

  { now send the length of the remainder, encoded as a 0 followed
    by the length in binary representation, to melclen bits }
  FImageInfo^.limit_reduce := melclen[COLOR] + 1;
  FBitIO.putbits(runlen, FImageInfo^.limit_reduce);

  { adjust melcoder parameters }
  if IsTrue(melcstate[COLOR]) then
    begin
      dec(melcstate[COLOR]);
      melclen[COLOR] := j[melcstate[COLOR]];
      melcorder[COLOR] := (1 shl melclen[COLOR]);
    end;

end;

procedure TJLSMelcode.close_process_run;
begin
  { retained for compatibility with ranked runs }
end;

end. 
 
 
 
