{ ****************************************************************************** }
{ * h264Stats.pas        by qq600585                                           * }
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

unit h264Stats;

{$INCLUDE zDefine.inc}
{$POINTERMATH ON}

interface

uses h264Types, CoreClasses;

type
  TFrameStats = class
  public
    pred: array [0 .. 8] of Int64_t;
    pred16: array [0 .. 3] of Int64_t;
    pred_8x8_chroma: array [0 .. 3] of Int64_t;
    ref: array [0 .. 15] of Int64_t;
    ptex_bits, itex_bits, mb_skip_count, mb_i4_count, mb_i16_count, mb_p_count: Int64_t;
    size_bytes: Int64_t;
    ssd: array [0 .. 2] of Int64_t;

    constructor Create;
    procedure Clear; virtual;
    procedure Add(a: TFrameStats);
  end;

  TStreamStats = class(TFrameStats)
  public
    i_count, p_count: Int64_t;
    procedure Clear; override;
  end;

implementation

procedure TStreamStats.Clear;
begin
  inherited Clear;
  i_count := 0;
  p_count := 0;
end;

constructor TFrameStats.Create;
begin
  Clear;
end;

procedure TFrameStats.Clear;
begin
  FillPtrByte(@pred, SizeOf(pred), 0);
  FillPtrByte(@pred16, SizeOf(pred16), 0);
  FillPtrByte(@pred_8x8_chroma, SizeOf(pred_8x8_chroma), 0);
  FillPtrByte(@ref, SizeOf(ref), 0);
  FillPtrByte(@ssd, SizeOf(ssd), 0);
  ptex_bits := 0;
  itex_bits := 0;
  mb_skip_count := 0;
  mb_i4_count := 0;
  mb_i16_count := 0;
  mb_p_count := 0;
  size_bytes := 0;
end;

procedure TFrameStats.Add(a: TFrameStats);
var
  i: int32_t;
begin
  inc(itex_bits, a.itex_bits);
  inc(ptex_bits, a.ptex_bits);
  inc(mb_i4_count, a.mb_i4_count);
  inc(mb_i16_count, a.mb_i16_count);
  inc(mb_p_count, a.mb_p_count);
  inc(mb_skip_count, a.mb_skip_count);
  inc(size_bytes, a.size_bytes);
  for i := 0 to 8 do
      inc(pred[i], a.pred[i]);
  for i := 0 to 3 do
      inc(pred16[i], a.pred16[i]);
  for i := 0 to 3 do
      inc(pred_8x8_chroma[i], a.pred_8x8_chroma[i]);
  for i := 0 to 15 do
      inc(ref[i], a.ref[i]);
  for i := 0 to 2 do
      inc(ssd[i], a.ssd[i]);
end;

end.  
 
 
