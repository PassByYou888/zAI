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
unit JLSJpegmark;

{$INCLUDE zDefine.inc}

interface

uses
  JLSGlobal,
  JLSBitIO,
  CoreClasses,
  MemoryStream64;

{ Marker identifiers }
const
  // publish marker
  JPEGLS_MARKER_SOI = $FFD8; { start of image } // modify by 600585@qq.com
  JPEGLS_MARKER_EOI = $FFD9; { end of image }   // modify by 600585@qq.com
  // custom marker
  JPEGLS_MARKER_SOI2 = $FF8D; { start of image } // modify by 600585@qq.com
  JPEGLS_MARKER_EOI2 = $FF9D; { end of image }   // modify by 600585@qq.com
  // publish marker
  JPEGLS_MARKER_SOS = $FFDA;  { Start of scan }
  JPEGLS_MARKER_DNL = $FFDC;  { Define number of lines }
  JPEGLS_MARKER_DRI = $FFDD;  { Define restart interval }
  JPEGLS_MARKER_RSTm = $FFD0; { Restart marker (FFD0-FFD7) }
  JPEGLS_MARKER_COM = $FFFE;  { Comment }

  { JPEG-LS specific }
  SOF_LS = $FFF7;    { Start of JPEG-LS regular frame }
  LSE = $FFF8;       { JPEG-LS extension marker }
  LSE_PARAMS = 1;    { Marker type within LSE - parameters }
  LSE_MAPTABLE = 2;  { Marker type within LSE - map tables }
  LSE_XMAPTABLE = 3; { Marker type within LSE - map table continuation }
  LSE_XY = 4;        { Marker type within LSE - image dimensions }

type
  TJLSJpegMark = class
  private
    FBitIO: TJLSBitIO;
    FImageInfo: PImageInfo;
  public
    constructor Create(ABitIO: TJLSBitIO; AImageInfo: PImageInfo);

    { Functions to write markers }
    function write_n_bytes(outstrm: TCoreClassStream; Value, n: Int): Int;
    function write_2_bytes(outstrm: TCoreClassStream; Value: Int): Int;
    function write_marker(outstrm: TCoreClassStream; Marker: Int): Int;
    function write_jpegls_frame(outstrm: TCoreClassStream; JP: pjpeg_ls_header): Int;
    function write_jpegls_scan(outstrm: TCoreClassStream; JP: pjpeg_ls_header): Int;
    function write_jpegls_extmarker(outstrm: TCoreClassStream; JP: pjpeg_ls_header; IDtype: Int): Int;
    function write_jpegls_restartmarker(outstrm: TCoreClassStream; JP: pjpeg_ls_header): Int;

    { Functions to read markers }
    function read_n_bytes(instrm: TCoreClassStream; n: Int): uint;
    function read_marker(instrm: TCoreClassStream; mkp: pint): Int;
    function seek_marker(instrm: TCoreClassStream; mkp: pint): Int;
    function read_jpegls_frame(instrm: TCoreClassStream; JP: pjpeg_ls_header): Int;
    function read_jpegls_scan(instrm: TCoreClassStream; JP: pjpeg_ls_header): Int;
    function read_jpegls_extmarker(instrm: TCoreClassStream; JP: pjpeg_ls_header): Int;
    function read_jpegls_restartmarker(instrm: TCoreClassStream; JP: pjpeg_ls_header): Int;
  end;

implementation


procedure check_range(Param: Int; Name: string; low, high: Int);
begin
  if (Param < low) or (Param > high) then
    begin
      RaiseInfo('Allowed range for %s is [%d..%d]: got %d', [Name, low, high, Param]);
      Exit;
    end;
end;

{ *
  *
  *   Marker output functions
  *
  * }

function TJLSJpegMark.write_n_bytes(outstrm: TCoreClassStream; Value, n: Int): Int;
var
  L: Int;
  i: Byte;
begin

  if (n > 4) then
    begin
      RaiseInfo('write_n_bytes: Only 32 bits variables supported.');
      Result := 10;
      Exit;
    end;

  for L := n - 1 downto 0 do
    begin
      i := shr_c(Value, 8 * L) and $000000FF;
      outstrm.write(i, 1 { sizeof(i) } );
    end;
  Result := n;
end;

function TJLSJpegMark.write_2_bytes(outstrm: TCoreClassStream; Value: Int): Int;
begin
  Result := write_n_bytes(outstrm, Value, 2);
end;

function TJLSJpegMark.write_marker(outstrm: TCoreClassStream; Marker: Int): Int;
{ Write a two-byte marker (just the marker identifier) }
begin
  write_n_bytes(outstrm, Marker, 2);
  Result := 2;
end;

function TJLSJpegMark.write_jpegls_frame(outstrm: TCoreClassStream; JP: pjpeg_ls_header): Int;
var
  i, marker_len,
    bpp, ct: Int;
  SX, SY: Int;
begin
  ct := 0;

  ct := ct + write_marker(outstrm, SOF_LS); { write JPEG-LS frame marker }

  check_range(JP^.comp, 'frame components', 1, 255);
  marker_len := 8 + 3 * JP^.comp;

  ct := ct + write_n_bytes(outstrm, marker_len, 2); { write marker length }
  bpp := 1;
  while ((1 shl bpp) < JP^.alp) do
      inc(bpp);

  ct := ct + write_n_bytes(outstrm, bpp, 1); { write bits/sample }

  { current implementation only supports up to 64K samples in
    either direction. Also, they must be specified in the frame header }
  check_range(JP^.Rows, 'rows', 1, 65535);
  check_range(JP^.columns, 'columns', 1, 65535);

  ct := ct + write_n_bytes(outstrm, JP^.Rows, 2);    { write number of rows }
  ct := ct + write_n_bytes(outstrm, JP^.columns, 2); { write number of cols }

  ct := ct + write_n_bytes(outstrm, JP^.comp, 1);

  { now write a triplet of bytes per component }
  for i := 0 to pred(JP^.comp) do
    begin
      SX := JP^.samplingx[i];
      SY := JP^.samplingy[i];

      check_range(SX, 'sampling(x)', 1, 4);
      check_range(SY, 'sampling(y)', 1, 4);
      ct := ct + write_n_bytes(outstrm, JP^.comp_ids[i], 1);  { component identifier }
      ct := ct + write_n_bytes(outstrm, (SX shl 4) or SY, 1); { sampling rates }
      ct := ct + write_n_bytes(outstrm, 0, 1);                { Tq unused }
    end;

  Result := ct;
end;

function TJLSJpegMark.write_jpegls_scan(outstrm: TCoreClassStream; JP: pjpeg_ls_header): Int;
var
  i, marker_len, ct: Int;

begin
  ct := 0;

  ct := ct + write_marker(outstrm, JPEGLS_MARKER_SOS); { write JPEG-LS scan marker }

  check_range(JP^.comp, 'scan components', 1, 4);

  if (JP^.comp = 1) and (JP^.color_mode <> PLANE_INT) then
    begin
      RaiseInfo('Interleave for 1 component must be PLANE_INT: got %d', [JP^.color_mode]);
      Result := 10;
      Exit;
    end;

  if (JP^.comp > 1) and (JP^.color_mode = 0) then
    begin
      RaiseInfo('Interleave for multi-component scan must be nonzero: got %d', [JP^.color_mode]);
      Result := 10;
      Exit;
    end;

  marker_len := 6 + 2 * JP^.comp;

  ct := ct + write_n_bytes(outstrm, marker_len, 2); { write marker length }
  ct := ct + write_n_bytes(outstrm, JP^.comp, 1);   { # of components for the scan }

  { write 2 bytes per component }
  for i := 0 to pred(JP^.comp) do
    begin
      ct := ct + write_n_bytes(outstrm, JP^.comp_ids[i], 1); { component identifier }
      ct := ct + write_n_bytes(outstrm, 0, 1);               { no tables in this implementation }
    end;

  check_range(JP^._near, '_near', 0, 255);
  ct := ct + write_n_bytes(outstrm, JP^._near, 1);

  check_range(JP^.color_mode, 'INTERLEAVE', 0, 2);
  ct := ct + write_n_bytes(outstrm, JP^.color_mode, 1);

  check_range(JP^.Shift, 'SHIFT', 0, 15);
  ct := ct + write_n_bytes(outstrm, JP^.Shift, 1);

  Result := ct;
end;

function TJLSJpegMark.write_jpegls_extmarker(outstrm: TCoreClassStream; JP: pjpeg_ls_header; IDtype: Int): Int;
var
  marker_len, ct: Int;
  TID,          { Table ID }
  WT,           { Width of table entries }
  MAXTAB,       { Maximum index of table }
  length: uint; { Marker length }
  i: Int;
begin
  ct := 0;

  { For Type 1 - non default parameters }
  if (IDtype = LSE_PARAMS) then
    begin
      ct := ct + write_marker(outstrm, LSE);             { write JPEG-LS extended marker id }
      ct := ct + write_n_bytes(outstrm, 13, 2);          { marker length }
      ct := ct + write_n_bytes(outstrm, LSE_PARAMS, 1);  { ext marker id }
      ct := ct + write_n_bytes(outstrm, JP^.alp - 1, 2); { MAXVAL }
      ct := ct + write_n_bytes(outstrm, JP^.t1, 2);
      ct := ct + write_n_bytes(outstrm, JP^.t2, 2);
      ct := ct + write_n_bytes(outstrm, JP^.t3, 2);
      ct := ct + write_n_bytes(outstrm, JP^.res, 2);
    end;

  Result := ct;
end;

{ Writes the DRI header to the JLS file }
function TJLSJpegMark.write_jpegls_restartmarker(outstrm: TCoreClassStream; JP: pjpeg_ls_header): Int;
var
  ct: Int;
  Ri: Int;     { the restart interval (# of MCU's between markers) }
  length: Int; { the length of the DRI header }
begin
  ct := 0;
  Ri := JP^.restart_interval;

  if (Ri <= 65535) then
      length := 4
  else
      length := 6;

  ct := ct + write_marker(outstrm, JPEGLS_MARKER_DRI);
  ct := ct + write_n_bytes(outstrm, length, 2);
  ct := ct + write_n_bytes(outstrm, Ri, 2);

  Result := ct;
end;

{ *
  *
  *   Marker input functions
  *
  * }

function TJLSJpegMark.seek_marker(instrm: TCoreClassStream; mkp: pint): Int;
{ Seeks a marker in the input stream. Returns the marker head, or EOF }
var
  c, c2, ct: Int;
begin
  ct := 0;
  c := FBitIO.mygetc;
  while (c <> BUF_EOF) do
    begin
      inc(ct);
      if (c = $FF) then
        begin
          c2 := FBitIO.mygetc;
          if (c2 = BUF_EOF) then
            begin
              Result := BUF_EOF;
              Exit;
            end;

          inc(ct);

          if IsTrue(c2 and $80) then
            begin
              mkp^ := (c shl 8) or c2;
              Result := ct;
              Exit;
            end;
        end;

      c := FBitIO.mygetc;
    end;
  Result := BUF_EOF;
end;

function TJLSJpegMark.read_n_bytes(instrm: TCoreClassStream; n: Int): uint;
{ reads n bytes (0 <= n <= 4) from the input stream }
var
  M: uint;
  i: Int;
begin
  M := 0;
  for i := 0 to pred(n) do
      M := (M shl 8) or FBitIO.mygetc;

  Result := M;
end;

function TJLSJpegMark.read_marker(instrm: TCoreClassStream; mkp: pint): Int;
{ reads a marker from the next two bytes in the input stream }
var
  M, ct: uint;
begin
  ct := 0;

  M := read_n_bytes(instrm, 2);
  if ((M and $FF00) <> $FF00) then
    begin
      RaiseInfo('read_marker: Expected marker, got %04x\n', [M]);
      Result := 10;
      Exit;
    end;
  mkp^ := M;
  Result := 2;
end;

function TJLSJpegMark.read_jpegls_frame(instrm: TCoreClassStream; JP: pjpeg_ls_header): Int;
{ reads the JPEG-LS frame marker (not including marker head) }
var
  i, marker_len, bpp, tq, comp, ct: Int;
  SX, SY, cid: Int;
begin
  ct := 0;

  { Read Marker Length }
  marker_len := read_n_bytes(instrm, 2);
  inc(ct, 2);

  { Read the bits per pixel }
  bpp := read_n_bytes(instrm, 1);
  inc(ct);

  check_range(bpp, 'bpp', 2, 16);
  JP^.alp := 1 shl bpp;

  { Read the rows and columns }
  JP^.Rows := read_n_bytes(instrm, 2);
  inc(ct, 2);
  JP^.columns := read_n_bytes(instrm, 2);
  inc(ct, 2);

  { Read component information }
  comp := read_n_bytes(instrm, 1);
  inc(ct);
  check_range(comp, 'COMP', 1, 255);
  JP^.comp := comp;

  for i := 0 to pred(comp) do
    begin

      cid := read_n_bytes(instrm, 1);
      inc(ct);
      SX := read_n_bytes(instrm, 1);
      inc(ct);
      tq := read_n_bytes(instrm, 1);
      inc(ct);
      check_range(tq, 'Tq', 0, 0);
      SY := SX and $0F;
      SX := shr_c(SX, 4);
      check_range(SX, 'sampling(x)', 1, 4);
      check_range(SY, 'sampling(y)', 1, 4);
      JP^.samplingx[i] := SX;
      JP^.samplingy[i] := SY;
      JP^.comp_ids[i] := cid;
    end;

  { Check for errors }
  if (marker_len <> 8 + 3 * comp) then
    begin
      Result := 10;
      RaiseInfo('read_jpegls_frame: inconsistent marker length: expected %d, got %d', [marker_len, 8 + 3 * comp]);
      Exit;
    end;

  Result := ct;
end;

{ reads the JPEG-LS scan marker (not including marker head) }
function TJLSJpegMark.read_jpegls_scan(instrm: TCoreClassStream; JP: pjpeg_ls_header): Int;
var
  i, marker_len, comp, ct: Int;
  cid, TM: Int;
begin
  ct := 0;

  marker_len := read_n_bytes(instrm, 2);
  inc(ct, 2);

  comp := read_n_bytes(instrm, 1);
  inc(ct, 1);
  check_range(comp, 'scan components', 1, 4);

  JP^.comp := comp;

  { read 2 bytes per component }
  for i := 0 to pred(comp) do
    begin

      cid := read_n_bytes(instrm, 1); { component identifier }
      inc(ct);
      TM := read_n_bytes(instrm, 1); { table identifier }
      inc(ct);

      if (IsTrue(TM)) then
        begin
          Result := 10;
          RaiseInfo('read_jpegls_scan: found nonzero table identifier, not supported');
          Exit;
        end;

      JP^.comp_ids[i] := cid;
    end;

  JP^._near := read_n_bytes(instrm, 1);
  inc(ct);
  check_range(JP^._near, '_near', 0, 255);

  JP^.color_mode := read_n_bytes(instrm, 1);
  inc(ct);
  check_range(JP^.color_mode, 'INTERLEAVE', 0, 2);

  if (JP^.comp = 1) and (JP^.color_mode <> 0) then
    begin
      {
        fprintf(stderr,"Interleave for 1 component must be 0: got %d\n",
        jp->color_mode);
      }

      { ignore interleave value, set to 0 }
      JP^.color_mode := 0;
    end;

  if (JP^.comp > 1) and (JP^.color_mode = 0) then
    begin
      Result := 10;
      RaiseInfo('Interleave for multi-component scan must be nonzero: got %d', [JP^.color_mode]);
      Exit;
    end;

  JP^.Shift := read_n_bytes(instrm, 1);
  inc(ct);
  check_range(JP^.Shift, 'SHIFT', 0, 15);

  if (marker_len <> 6 + 2 * comp) then
    begin
      Result := 10;
      RaiseInfo('read_jpegls_scan: inconsistent marker length: expected %d, got %d', [marker_len, 6 + 2 * comp]);
    end;
  Result := ct;
end;

{ reads the JPEG-LS extension marker (not including marker head) }
{ Supports non-default type (1) and mapping table type (2) }
constructor TJLSJpegMark.Create(ABitIO: TJLSBitIO; AImageInfo: PImageInfo);
begin
  FBitIO := ABitIO;
  FImageInfo := AImageInfo;
end;

function TJLSJpegMark.read_jpegls_extmarker(instrm: TCoreClassStream; JP: pjpeg_ls_header): Int;
var
  marker_len, { marker length }
  MaxVal,     { max value }
  t1, t2, t3, { thresholds }
  IDtype,     { LSE type }
  TID,        { table ID }
  WT,         { width of each table entry }
  MAXTAB,     { maximum table index }
  i, ct: Int;
begin
  ct := 0;

  { Read marker length }
  marker_len := read_n_bytes(instrm, 2); { marker length }
  inc(ct, 2);

  { Read id type }
  IDtype := read_n_bytes(instrm, 1);
  inc(ct, 1);

  { For Type 1 - non default parameters }
  if (IDtype = LSE_PARAMS) then
    begin
      if (marker_len <> 13) then
        begin
          RaiseInfo('read_jpegls_extmarker: bad marker length %d', [marker_len]);
          Result := 10;
          Exit;
        end;

      { read maxval }
      MaxVal := read_n_bytes(instrm, 2);
      inc(ct, 2);
      JP^.alp := MaxVal + 1;

      { read thresholds and reset }
      JP^.t1 := read_n_bytes(instrm, 2);
      inc(ct, 2);
      JP^.t2 := read_n_bytes(instrm, 2);
      JP^.t3 := read_n_bytes(instrm, 2);
      JP^.res := read_n_bytes(instrm, 2);
      inc(ct, 6);

      Result := ct;
      Exit;
    end;

  { For Type 2 - mapping table }
  if (IDtype = LSE_MAPTABLE) then
    begin

      { Indicate table used }
      JP^.need_table := 1;

      { Read table ID }
      TID := read_n_bytes(instrm, 1);
      JP^.TID := TID;
      inc(ct, 1);

      { Read width of table entry }
      WT := read_n_bytes(instrm, 1);
      JP^.WT := WT;
      if (WT <= 0) or (WT > 3) then
        begin
          RaiseInfo('Width of mapping table entries must be either 1,2 or 3 in this implementation. Sorry!');
          Result := 0;
          Exit;
        end;
      inc(ct, 1);

      { Calculate value of MAXTAB }
      MAXTAB := ((marker_len - 5) div WT) - 1;
      JP^.MAXTAB := MAXTAB;

      { Get table entries }
      JP^.Table^[TID] := safecalloc((MAXTAB + 1) * SizeOf(Int), 1);
      for i := 0 to MAXTAB do
        begin
          pwordarray(JP^.Table^[TID])^[i] := read_n_bytes(instrm, WT);
        end;
      inc(ct, (MAXTAB + 1) * WT);

      Result := ct;
      Exit;
    end;

  { Non supported types }
  Result := 0;
  RaiseInfo('LSE marker type %i not supported in this implementation', [IDtype]);
end;

{ Read DRI restart marker }
function TJLSJpegMark.read_jpegls_restartmarker(instrm: TCoreClassStream; JP: pjpeg_ls_header): Int;
var
  ct: Int;
  marker_len: Int; { the marker length }
  Ri: Int;         { the restart interval }
begin
  ct := 0;

  { Read marker length }
  marker_len := read_n_bytes(instrm, 2);
  inc(ct, 2);

  { Read the restart interval }
  Ri := read_n_bytes(instrm, marker_len - 2);
  inc(ct, (marker_len - 2));

  JP^.restart_interval := Ri;

  Result := ct;
end;

end.
