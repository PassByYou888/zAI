{ ****************************************************************************** }
{ * h264Pixel.pas        by qq600585                                           * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

unit h264Pixel;

{$INCLUDE zDefine.inc}
{$POINTERMATH ON}

interface

uses
  h264Types, h264Util, CoreClasses;

procedure pixel_load_4x4(dest, Src: uint8_p; stride: int32_t);
procedure pixel_save_4x4(Src, dest: uint8_p; stride: int32_t);
procedure pixel_init;

var
  sad_16x16, sad_8x8, sad_4x4, ssd_16x16, ssd_8x8, satd_4x4, satd_8x8, satd_16x16: mbcmp_func_t;
  var_16x16: mbstat_func_t;
  pixel_load_16x16, pixel_loadu_16x16, pixel_load_8x8, pixel_save_16x16, pixel_save_8x8: pixmove_func_t;
  pixel_add_4x4, pixel_sub_4x4: pixoper_func_t;
  pixel_avg_16x16: pixavg_func_t;

implementation

function sad_16x16_pas(pix1, pix2: uint8_p; stride: int32_t): int32_t;
var
  x, y: int32_t;
begin
  Result := 0;
  for y := 0 to 15 do
    begin
      for x := 0 to 15 do
          inc(Result, Abs(pix1[x] - pix2[x]));
      inc(pix1, 16);
      inc(pix2, stride);
    end;
end;

function sad_8x8_pas(pix1, pix2: uint8_p; stride: int32_t): int32_t;
var
  x, y: int32_t;
begin
  Result := 0;
  for y := 0 to 7 do
    begin
      for x := 0 to 7 do
          inc(Result, Abs(pix1[x] - pix2[x]));
      inc(pix1, 16);
      inc(pix2, stride);
    end;
end;

function sad_4x4_pas(pix1, pix2: uint8_p; stride: int32_t): int32_t;
var
  x, y: int32_t;
begin
  Result := 0;
  for y := 0 to 3 do
    begin
      for x := 0 to 3 do
          inc(Result, Abs(pix1[x] - pix2[x]));
      inc(pix1, 16);
      inc(pix2, stride);
    end;
end;

function ssd_16x16_pas(pix1, pix2: uint8_p; stride: int32_t): int32_t;
var
  x, y: int32_t;
begin
  Result := 0;
  for y := 0 to 15 do
    begin
      for x := 0 to 15 do
          inc(Result, (pix1[x] - pix2[x]) * (pix1[x] - pix2[x]));
      inc(pix1, 16);
      inc(pix2, stride);
    end;
end;

function ssd_8x8_pas(pix1, pix2: uint8_p; stride: int32_t): int32_t;
var
  x, y: int32_t;
begin
  Result := 0;
  for y := 0 to 7 do
    begin
      for x := 0 to 7 do
          inc(Result, (pix1[x] - pix2[x]) * (pix1[x] - pix2[x]));
      inc(pix1, 16);
      inc(pix2, stride);
    end;
end;

function var_16x16_pas(pix: uint8_p): UInt32;
var
  x, y: int32_t;
  s: uint16_t;  // sum
  SS: uint32_t; // sum squared
begin
  s := 0;
  SS := 0;
  for y := 0 to 15 do
    begin
      for x := 0 to 15 do
        begin
          inc(s, pix[x]);
          inc(SS, pix[x] * pix[x]);
        end;
      inc(pix, 16);
    end;
  Result := SS - (s * s div 256);
end;

function satd_4x4_pas(pix1, pix2: uint8_p; stride: int32_t): int32_t;
type
  matrix_t = array [0 .. 3, 0 .. 3] of int16_t;
var
  a, t: matrix_t;
  E, f, g, h: array [0 .. 3] of int16_t;
  i, j: int32_t;
begin
  for i := 0 to 3 do
    begin
      for j := 0 to 3 do
          a[i][j] := pix1[j] - pix2[j];
      inc(pix1, 16);
      inc(pix2, stride);
    end;

  for i := 0 to 3 do
    begin
      E[i] := a[0][i] + a[2][i];
      f[i] := a[0][i] - a[2][i];
      g[i] := a[1][i] + a[3][i];
      h[i] := a[1][i] - a[3][i];

      t[i][0] := E[i] + g[i];
      t[i][1] := E[i] - g[i];
      t[i][2] := f[i] + h[i];
      t[i][3] := f[i] - h[i];
    end;

  for i := 0 to 3 do
    begin
      E[i] := t[0][i] + t[2][i];
      f[i] := t[0][i] - t[2][i];
      g[i] := t[1][i] + t[3][i];
      h[i] := t[1][i] - t[3][i];
    end;
  for i := 0 to 3 do
    begin
      t[i][0] := E[i] + g[i];
      t[i][1] := E[i] - g[i];
      t[i][2] := f[i] + h[i];
      t[i][3] := f[i] - h[i];
    end;

  Result := 0;
  for i := 0 to 15 do
      inc(Result, Abs(int16_p(@t)[i]));
end;

function satd_8x8_pas(pix1, pix2: uint8_p; stride: int32_t): int32_t;
var
  i: int32_t;
begin
  Result := 0;
  for i := 0 to 1 do
    begin
      inc(Result, satd_4x4_pas(pix1, pix2, stride));
      inc(Result, satd_4x4_pas(pix1 + 4, pix2 + 4, stride));
      inc(pix1, 4 * 16);
      inc(pix2, 4 * stride);
    end
end;

function satd_16x16_pas(pix1, pix2: uint8_p; stride: int32_t): int32_t;
var
  i: int32_t;
begin
  Result := 0;
  for i := 0 to 3 do
    begin
      inc(Result, satd_4x4_pas(pix1, pix2, stride));
      inc(Result, satd_4x4_pas(pix1 + 4, pix2 + 4, stride));
      inc(Result, satd_4x4_pas(pix1 + 8, pix2 + 8, stride));
      inc(Result, satd_4x4_pas(pix1 + 12, pix2 + 12, stride));
      inc(pix1, 4 * 16);
      inc(pix2, 4 * stride);
    end
end;

procedure pixel_sub_4x4_pas(pix1, pix2: uint8_p; Diff: int16_p);
var
  x, y: int32_t;
begin
  for y := 0 to 3 do
    begin
      for x := 0 to 3 do
          Diff[x] := pix1[x] - pix2[x];
      inc(pix1, 16);
      inc(pix2, 16);
      inc(Diff, 4);
    end;
end;

procedure pixel_add_4x4_pas(pix1, pix2: uint8_p; Diff: int16_p);

  function Clip(c: int32_t): uint8_t; inline;
  begin
    Result := uint8_t(c);
    if c > 255 then
        Result := 255
    else
      if c < 0 then
        Result := 0;
  end;

var
  y, x: int32_t;
begin
  for y := 0 to 3 do
    begin
      for x := 0 to 3 do
          pix1[x] := Clip(Diff[x] + pix2[x]);
      inc(pix1, 16);
      inc(pix2, 16);
      inc(Diff, 4);
    end;
end;

procedure pixel_load_16x16_pas(dest, Src: uint8_p; stride: int32_t);
var
  i: int32_t;
begin
  for i := 0 to 15 do
    begin
      CopyPtr(Src, dest, 16);
      inc(Src, stride);
      inc(dest, 16);
    end;
end;

procedure pixel_load_8x8_pas(dest, Src: uint8_p; stride: int32_t);
var
  i: int32_t;
begin
  for i := 0 to 7 do
    begin
      uint64_p(dest)^ := uint64_p(Src)^;
      inc(Src, stride);
      inc(dest, 16);
    end;
end;

procedure pixel_load_4x4(dest, Src: uint8_p; stride: int32_t);
var
  i: int32_t;
begin
  for i := 0 to 3 do
    begin
      uint32_p(dest)^ := uint32_p(Src)^;
      inc(Src, stride);
      inc(dest, 16);
    end;
end;

procedure pixel_save_16x16_pas(Src, dest: uint8_p; stride: int32_t);
var
  i: int32_t;
begin
  for i := 0 to 15 do
    begin
      CopyPtr(Src, dest, 16);
      inc(dest, stride);
      inc(Src, 16);
    end;
end;

procedure pixel_save_8x8_pas(Src, dest: uint8_p; stride: int32_t);
var
  i: int32_t;
begin
  for i := 0 to 7 do
    begin
      uint64_p(dest)^ := uint64_p(Src)^;
      inc(dest, stride);
      inc(Src, 16);
    end;
end;

procedure pixel_save_4x4(Src, dest: uint8_p; stride: int32_t);
var
  i: int32_t;
begin
  for i := 0 to 3 do
    begin
      uint32_p(dest)^ := uint32_p(Src)^;
      inc(dest, stride);
      inc(Src, 16);
    end;
end;

procedure pixel_avg_16x16_pas(src1, src2, dest: uint8_p; stride: int32_t);
var
  x, y: int32_t;
begin
  for y := 0 to 15 do
    begin
      for x := 0 to 15 do
          dest[x] := (src1[x] + src2[x] + 1) shr 1;
      inc(src1, stride);
      inc(src2, stride);
      inc(dest, 16);
    end;
end;

procedure pixel_init;
begin
  sad_16x16 := @sad_16x16_pas;
  sad_8x8 := @sad_8x8_pas;
  sad_4x4 := @sad_4x4_pas;

  ssd_16x16 := @ssd_16x16_pas;
  ssd_8x8 := @ssd_8x8_pas;
  var_16x16 := @var_16x16_pas;

  satd_4x4 := @satd_4x4_pas;
  satd_8x8 := @satd_8x8_pas;
  satd_16x16 := @satd_16x16_pas;

  pixel_load_16x16 := @pixel_load_16x16_pas;
  pixel_loadu_16x16 := @pixel_load_16x16_pas;
  pixel_load_8x8 := @pixel_load_8x8_pas;
  pixel_save_16x16 := @pixel_save_16x16_pas;
  pixel_save_8x8 := @pixel_save_8x8_pas;
  pixel_add_4x4 := @pixel_add_4x4_pas;
  pixel_sub_4x4 := @pixel_sub_4x4_pas;
  pixel_avg_16x16 := @pixel_avg_16x16_pas;
end;

end.
