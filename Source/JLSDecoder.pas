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
unit JLSDecoder;

{$INCLUDE zDefine.inc}

interface

uses
  CoreClasses, PascalStrings, MemoryStream64,
  JLSGlobal, JLSJpegmark, JLSBitIO, JLSMelcode, JLSLossless, JLSLossy, JLSBaseCodec;

type
  TJLSDecoder = class(TJLSBaseCodec)
  public
    Shift: Int;
    got_lse: Int;     { got an LSE marker }
    got_table: Int;   { got a mapping table }
    got_restart: Int; { got a restart marker indicatino }

    procedure initbuffers(comp: Int);
    procedure write_one_line(Line: ppixel; Cols: Int; outfile: TCoreClassStream);
    procedure closebuffers;
    function Initialize: Int;
  public
    function Execute: Boolean; override;
    procedure swaplines;
  end;

implementation

{ Write one row of pixel values }

uses DoStatusIO;

procedure TJLSDecoder.write_one_line(Line: ppixel; Cols: Int; outfile: TCoreClassStream);
var
  i, index: Integer;
  maptable: pwordarray;
  line8: TBytes;
  line8_3: PByteArray;
  line16: pwordarray;
  b: Byte;
begin
  { No mapping tables used }
  if (not(IsTrue(head_scan[0]^.need_table))) then
    begin
      if (FImageInfo.bpp16 = False) then
        begin
          SetLength(line8, Cols);
          for i := 0 to pred(Cols) do
            begin
              line8[i] := ENDIAN8(Line^);
              inc(Line);
            end;
          outfile.write(line8[0], Cols);
        end
      else
        begin
          outfile.write(Line^, Cols * SizeOf(Pixel));
        end;
    end
    { Mapping tables used }
  else
    begin
      if (FImageInfo.bpp16 = False) then
        begin
          { Write one byte per table entry }
          if (head_scan[0]^.WT = 1) then
            begin
              SetLength(line8, Cols);
              // line8 := safealloc(cols);  { If don't have 2, it mallocs over the table? }

              maptable := head_scan[0]^.Table^[head_scan[0]^.TID];

              for i := 0 to pred(Cols) do
                begin
                  index := PByteArray(Line)^[i];
                  line8[i] := ENDIAN8(maptable^[index]);
                end;

              outfile.write(line8[0], Cols);

              SetLength(line8, 0);
            end
            { Write two bytes per table entry }
          else if (head_scan[0]^.WT = 2) then
            begin
              line16 := safealloc(Cols * 2);

              maptable := head_scan[0]^.Table^[head_scan[0]^.TID];

              for i := 0 to pred(Cols) do
                begin
                  index := pwordarray(Line)^[i];
                  line16^[i] := maptable^[index];
                end;

              outfile.write(line16^[0], Cols * SizeOf(short));

              FreeMem(line16);
            end
            { Write three bytes per table entry }
          else if (head_scan[0]^.WT = 3) then
            begin
              line8_3 := safealloc(Cols * 3);

              maptable := head_scan[0]^.Table^[head_scan[0]^.TID];

              for i := 0 to pred(Cols) do
                begin
                  index := PByteArray(Line)^[i];
                  line8_3^[i * 3] := shr_c(maptable^[index], 16);
                  line8_3^[(i * 3) + 1] := shr_c(maptable^[index], 8);
                  line8_3^[(i * 3) + 2] := maptable^[index];
                end;

              outfile.write(line8_3, Cols * 3);

              FreeMem(line8_3);
            end;

          { Can't do 16 bit index values }
        end
      else
        begin
          RaiseInfo('Sorry, mapping tables are only supported for 8 bpp images in this implementation.');
          // exit(0);
        end;
    end;

end;

procedure TJLSDecoder.initbuffers(comp: Int);
var
  PTR: ppixel;
begin
  pscanl0 := safecalloc(comp * (width + LEFTMARGIN + RIGHTMARGIN + NEGBUFFSIZE), SizeOf(Pixel));
  cscanl0 := safecalloc(comp * (width + LEFTMARGIN + RIGHTMARGIN + NEGBUFFSIZE), SizeOf(Pixel));

  { Adjust scan line pointers taking into account the margins,
    and also the fact that indexing for scan lines starts from 1
    (this will probably have to be changed in the future)
  }
  PTR := ppixel(pscanl0);
  inc(PTR, comp * (LEFTMARGIN - 1));
  pscanline := PTR;

  PTR := ppixel(cscanl0);
  inc(PTR, comp * (LEFTMARGIN - 1));
  cscanline := PTR;

  FBitIO.createzeroLUT();
end;

procedure TJLSDecoder.swaplines;
var
  Temp, PTR: ppixel;
begin
  Temp := pscanl0;
  pscanl0 := cscanl0;
  cscanl0 := Temp;
  PTR := pscanl0;
  inc(PTR, FImageInfo.Components * (LEFTMARGIN - 1));
  pscanline := PTR;

  PTR := cscanl0;
  inc(PTR, FImageInfo.Components * (LEFTMARGIN - 1));
  cscanline := PTR;
end;

procedure TJLSDecoder.closebuffers;
begin
  FreeMem(pscanl0);
  FreeMem(cscanl0);
end;

function TJLSDecoder.Initialize: Int;
var
  color_mode_string: string;

  i, max_samp_columns, max_samp_rows, mk, n_s,
    end_of_seek,
    seek_return,
    gotinf,
    gotoutf,
    Pos: Int; { position in the file, after the header }
  alpha_temp: Int;
begin
  end_of_seek := 0;
  gotinf := 0;
  gotoutf := 0;

  { Read the compressed image frame header }
  FBitIO.bufiinit;

  head_frame := safecalloc(1, SizeOf(tjpeg_ls_header));

  for n_s := 0 to pred(MAX_SCANS) do
    begin
      head_scan[n_s] := safecalloc(1, SizeOf(tjpeg_ls_header));
      head_scan[n_s]^.t1 := 0;
      head_scan[n_s]^.t2 := 0;
      head_scan[n_s]^.t3 := 0;
      head_scan[n_s]^.res := DEFAULT_RESET;
    end;

  { Read SOI }
  seek_return := FJpeg.seek_marker(FInputStream, @mk);
  if (seek_return = BUF_EOF) then
    begin
      DoStatus('*** Premature End of File seeking SOI');
      Result := 10;
      Exit;
    end
  else begin
      Pos := seek_return;
      if (mk <> JPEGLS_MARKER_SOI) and (mk <> JPEGLS_MARKER_SOI2) then
        begin
          DoStatus('Marker %04x found: first marker must be SOI (%04x) in this implementation', [mk, JPEGLS_MARKER_SOI]);
          Result := 10;
          Exit;
        end;
    end;

  { Read SOF_LS }
  seek_return := FJpeg.seek_marker(FInputStream, @mk);
  if (seek_return = BUF_EOF) then
    begin
      DoStatus('*** Premature End of File seeking SOF_LS');
      Result := 10;
      Exit;
    end
  else begin
      Pos := Pos + seek_return; { Read SOF_LS }
      if (mk <> SOF_LS) then
        begin
          DoStatus('Marker %04x found: second marker must be SOF_LS (%04x) in this implementation', [mk, SOF_LS]);
          Result := 10;
          Exit;
        end;
    end;

  { Read the frame header (SOF) }
  seek_return := FJpeg.read_jpegls_frame(FInputStream, head_frame);
  if (seek_return = BUF_EOF) then
    begin
      DoStatus('*** Premature End of File reading frame header');
      Result := 10;
      Exit;
    end
  else
      Pos := Pos + seek_return;

  head_scan[0]^.alp := head_frame^.alp; { default alpha }

  { LSE Extension header }
  { This version supports only 2 extension headers, and 1 set of
    parameters for all the scans }
  got_lse := 0;
  head_scan[0]^.need_table := 0;

  while (not IsTrue(end_of_seek)) do
    begin
      seek_return := FJpeg.seek_marker(FInputStream, @mk);
      if (seek_return = BUF_EOF) then
        begin
          DoStatus('*** Premature End of File seeking SOS or LSE marker');
          Result := 10;
          Exit;
        end;
      Pos := Pos + seek_return;

      case mk of

        LSE:
          begin
            seek_return := FJpeg.read_jpegls_extmarker(FInputStream, head_scan[0]);
            if (seek_return = BUF_EOF) then
              begin
                DoStatus('*** Premature End of File');
                Result := 10;
                Exit;
              end;
            Pos := Pos + seek_return;
            got_lse := 1;
          end;

        JPEGLS_MARKER_DRI:
          begin
            seek_return := FJpeg.read_jpegls_restartmarker(FInputStream, head_scan[0]);
            Pos := Pos + seek_return;
            got_restart := 1;
            restart_interval := head_scan[0]^.restart_interval;
          end;

        JPEGLS_MARKER_SOS:
          end_of_seek := 1;
      end;
    end;
  { End of extension header }

  { Read the scan header }
  seek_return := FJpeg.read_jpegls_scan(FInputStream, head_scan[0]);
  if (seek_return = BUF_EOF) then
    begin
      DoStatus('*** Premature End of File reading scan marker segment');
      Result := 10;
      Exit;
    end;

  Pos := Pos + seek_return;

  Shift := head_scan[0]^.Shift;
  if (Shift <> 0) then
    begin
      DoStatus('Got shift = %d != 0 : not implemented.', [Shift]);
      Result := 10;
      Exit;
    end;

  FImageInfo._near := head_scan[0]^._near;
  color_mode := head_scan[0]^.color_mode;
  width := head_frame^.columns;
  height := head_frame^.Rows;
  alpha0 := head_scan[0]^.alp;
  head_frame^.alp := alpha0;
  FImageInfo.Components := head_frame^.comp;

  if (color_mode = PLANE_INT) then
      number_of_scans := FImageInfo.Components
  else
      number_of_scans := 1;

  set_thresholds(head_scan[0]^.alp, head_scan[0]^._near, @(head_scan[0]^.t1), @(head_scan[0]^.t2), @(head_scan[0]^.t3));
  FT1 := head_scan[0]^.t1;
  FT2 := head_scan[0]^.t2;
  FT3 := head_scan[0]^.t3;

  if (head_scan[0]^.res <> DEFAULT_RESET) then
    begin
      DoStatus('ERROR: Version compiled for fixed RESET=%d parameter: got %d', [DEFAULT_RESET, head_scan[0]^.res]);
      Result := 10;
      Exit;
    end;

  { Check to see if lossless or lossy }
  if (FImageInfo._near = 0) then
      lossy := False
  else
      lossy := True;

  { Check for 16 or 8 bit mode }
  if (alpha0 <= MAXA16) and (alpha0 > MAXA8) then
    begin
      { 16 bit }
      FImageInfo.bpp16 := True;
      lutmax := LUTMAX16;
    end
  else if (alpha0 <= MAXA8) and (alpha0 >= 1) then
    begin
      { 8 bit }
      FImageInfo.bpp16 := False;
      lutmax := LUTMAX8;
    end
  else begin
      DoStatus('Got alpha = %d', [alpha0 + 1]);
      RaiseInfo('Bad value for alpha. Sorry...');
      Result := 10;
      Exit;
    end;

  check_compatibility(head_frame, head_scan[0], 0);

  for i := 0 to pred(FImageInfo.Components) do
    begin
      samplingx[i] := head_frame^.samplingx[i];
      samplingy[i] := head_frame^.samplingy[i];
      if (((samplingx[i] <> 1) or (samplingy[i] <> 1)) and (FImageInfo.Components > 1)) then
        begin
          Result := 10;
          Exit;
        end;
    end;

  { Compute the image size for the different components }
  if (FImageInfo.Components = 1) then
    begin
      whose_max_size_rows := 0;
      samplingy[0] := 1;
    end
  else
    begin
      max_samp_columns := 0;
      max_samp_rows := 0;
      for i := 0 to pred(FImageInfo.Components) do
        begin
          if (samplingx[i] > max_samp_columns) then
            begin
              max_samp_columns := samplingx[i];
              whose_max_size_columns := i;
            end;
          if (samplingy[i] > max_samp_rows) then
            begin
              max_samp_rows := samplingy[i];
              whose_max_size_rows := i;
            end;
        end;

      for i := 0 to pred(FImageInfo.Components) do
        begin
          if (i <> whose_max_size_columns) then
            begin
              c_columns[i] := c_columns[whose_max_size_columns] * samplingx[i];
              c_columns[i] := c_columns[i] div samplingx[whose_max_size_columns];
            end;
          if (i <> whose_max_size_rows) then
            begin
              c_rows[i] := c_rows[whose_max_size_rows] * samplingy[i];
              c_rows[i] := c_rows[i] div samplingy[whose_max_size_rows];
            end;
        end;
    end;

  { check that color mode is valid and pick color mode string }
  case color_mode of
    PLANE_INT: color_mode_string := plane_int_string;
    LINE_INT: color_mode_string := line_int_string;
    PIXEL_INT: color_mode_string := pixel_int_string;
    else
      begin
        DoStatus('ERROR: Invalid color mode %d', [color_mode]);
        Result := 10;
        Exit;
      end;
  end;

  FImageInfo.alpha := alpha0;
  FImageInfo.ceil_half_alpha := (FImageInfo.alpha + 1) div 2;

  FImageInfo.highmask := -FImageInfo.alpha;
  { check that alpha is a power of 2 }
  alpha0 := FImageInfo.alpha;
  i := -1;
  while IsTrue(alpha0) do
    begin
      alpha0 := shr_c(alpha0, 1);
      inc(i);
    end;

  if (FImageInfo.alpha <> (1 shl i)) then
    begin
      DoStatus('Sorry, this version has been optimized for alphabet size = power of 2, got %d', [FImageInfo.alpha]);
      Result := 10;
      Exit;
    end;

  if (lossy = True) then
    begin
      { compute auxiliary parameters for _near-lossless (globals) }
      quant := 2 * FImageInfo._near + 1;
      FImageInfo.qbeta := (FImageInfo.alpha + 2 * FImageInfo._near + quant - 1) div quant;
      FImageInfo.beta := quant * FImageInfo.qbeta;
      ceil_half_qbeta := (FImageInfo.qbeta + 1) div 2;
      FImageInfo.negNEAR := -FImageInfo._near;
      FImageInfo.alpha1eps := FImageInfo.alpha - 1 + FImageInfo._near;
      DoStatus('_near-lossless mode: _near = %d  beta = %d  qbeta = %d', [FImageInfo._near, FImageInfo.beta, FImageInfo.qbeta]);
    end;

  { compute bits per sample for input symbols }

  bpp := 1;
  while (1 shl bpp) < FImageInfo.alpha do
      inc(bpp);

  { compute bits per sample for unencoded prediction errors }
  if (lossy = True) then
    begin
      FImageInfo.qbpp := 1;
      while (1 shl FImageInfo.qbpp) < FImageInfo.qbeta do
          inc(FImageInfo.qbpp);
    end
  else
      FImageInfo.qbpp := bpp;

  if (bpp < 2) then
      bpp := 2;

  { limit for unary part of Golomb code }
  if (bpp < 8) then
      FImageInfo.Limit := 2 * (bpp + 8) - FImageInfo.qbpp - 1
  else
      FImageInfo.Limit := 4 * bpp - FImageInfo.qbpp - 1;

  { print out parameters }
  if FEnableLog then
    begin
      DoStatus('Image: cols=%d rows=%d alpha=%d comp=%d mode=%d (%s)', [width, height, alpha0, Components, color_mode, color_mode_string]);
      DoStatus('Parameters: Ta=%d Tb=%d Tc=%d RESET=%d limit=%d', [t1, t2, t3, Reset, Limit]);
    end;

  { Allocate memory pools. }
  initbuffers(FImageInfo.Components);

  { return size of the header, in bytes }
  Result := Pos;
end;

{ Main loop for decoding files }

function TJLSDecoder.Execute: Boolean;
var
  n, n_c, n_r, my_i, n_s, mk, seek_return,
    found_EOF: Int;

  pos0, pos1, tot_in, tot_out: long;
  local_scanl0, local_scanl1, local_pscanline, local_cscanline: ppixel;
  MCUs_counted: Int;
  PTR: ppixel;
  ms: TMemoryStream64;
  i: Integer;
begin
  Result := False;
  inherited Execute;
  found_EOF := 0;
  tot_in := 0;
  tot_out := 0;
  local_scanl0 := nil;
  local_scanl1 := nil;

  { Parse the parameters, initialize }
  { Not yet fully implemented }
  FBitIO.bufiinit;

  local_scanl0 := nil;
  local_scanl1 := nil;

  pos0 := Initialize;

  { Initialize the scanline buffers }

  if (FImageInfo.Components > 1) then
    begin
      local_scanl0 := safecalloc(width + LEFTMARGIN + RIGHTMARGIN + NEGBUFFSIZE, SizeOf(Pixel));
      local_scanl1 := safecalloc(width + LEFTMARGIN + RIGHTMARGIN + NEGBUFFSIZE, SizeOf(Pixel));

      PTR := local_scanl0;
      inc(PTR, LEFTMARGIN - 1);
      local_pscanline := PTR;

      PTR := local_scanl1;
      inc(PTR, LEFTMARGIN - 1);
      local_cscanline := PTR;
    end;

  for n_s := 0 to pred(number_of_scans) do
    begin
      { Repeat prediction/quantization/statistics scanline by scanline,
        for each scan. }

      { Reset Restart Markers for every scan }
      MCUs_counted := 0;

      { This implementation supports parameters in 1st scan }
      if (n_s = 0) then
        begin
          { Prepare the quantization LUTs }
          prepareLUTs();

          if (lossy = True) then
              prepare_qtables(FImageInfo.alpha, FImageInfo._near); { prepare div/mul tables for _near-lossless quantization }

        end
      else
        begin { Read further scan headers }

          seek_return := FJpeg.seek_marker(FInputStream, @mk);
          if (seek_return = BUF_EOF) then
            begin
              DoStatus('*** Premature End of File seeking SOS marker.');
              Result := False;
              Exit;
            end;

          if (seek_return > 2) then
            begin
              DoStatus('*** WARNING: %d extra bytes between end of scan and next marker.', [seek_return - 2]);
              DoStatus('***          Added to marker segment count.');
              Result := False;
              Exit;
            end;

          pos0 := pos0 + seek_return;
          if (mk <> JPEGLS_MARKER_SOS) then
            begin
              DoStatus('Expecting SOS (%x), got %x', [JPEGLS_MARKER_SOS, mk]);
              Result := False;
              Exit;
            end;

          seek_return := FJpeg.read_jpegls_scan(FInputStream, head_scan[n_s]); { Read the scan header }
          if (seek_return = BUF_EOF) then
            begin
              DoStatus('*** Premature End of File reading scan marker segment');
              Result := False;
              Exit;
            end;
          pos0 := pos0 + seek_return;
          if (head_scan[n_s]^.Shift <> 0) then
            begin
              DoStatus('Got shift = %d != 0 : not implemented.', [head_scan[n_s]^.Shift]);
              Result := False;
              Exit;
            end;

          if (head_scan[n_s]^._near <> FImageInfo._near) then
            begin
              DoStatus('Got _near=%d after _near=%d: cannot change parameters between scans in this implementation.', [head_scan[n_s]^._near, FImageInfo._near]);
              Result := False;
              Exit;
            end;

          if ((head_scan[n_s]^.color_mode <> PLANE_INT) or (head_scan[n_s]^.comp <> 1) or
            (head_scan[n_s]^.comp_ids[0] <> n_s + 1)) then
            begin
              DoStatus('This implementation supports multiple scans only in PLANE INTERLEAVED mode.');
              Result := False;
              Exit;
            end;
        end;

      { Initializations for each scan }
      FBitIO.bitiinit();

      { Start from 1st image row }
      n := 0;

      { Initialize stats arrays }
      if (lossy = True) then
          init_stats(FImageInfo.qbeta)
      else
          init_stats(FImageInfo.alpha);

      { Initialize run processing }
      FMelcode.init_process_run;

      if (color_mode = LINE_INT) then { line int. }
        begin
          { ***********************************************************************/
            /*           Line interleaved mode with single file received           */
            /*********************************************************************** }

          if (lossy = False) then
            begin
              { LOSSLESS MODE }
              inc(n);
              while (n <= height) do
                begin
                  { 'extend' the edges }
                  for n_c := 0 to pred(FImageInfo.Components) do
                    begin
                      ppixelarray(cscanline)^[-FImageInfo.Components + n_c] := ppixelarray(pscanline)^[FImageInfo.Components + n_c];
                      ppixelarray(cscanline)^[n_c] := ppixelarray(pscanline)^[FImageInfo.Components + n_c];
                    end;

                  for n_c := 0 to pred(FImageInfo.Components) do
                    begin
                      if (FImageInfo.Components > 1) then
                        begin
                          for my_i := 0 to pred(width + LEFTMARGIN + RIGHTMARGIN) do
                            begin
                              ppixelarray(local_cscanline)^[-1 + my_i] := ppixelarray(cscanline)^[-FImageInfo.Components + my_i * FImageInfo.Components + n_c];
                              ppixelarray(local_pscanline)^[-1 + my_i] := ppixelarray(pscanline)^[-FImageInfo.Components + my_i * FImageInfo.Components + n_c];
                            end;
                        end
                      else
                        begin
                          local_cscanline := cscanline;
                          local_pscanline := pscanline;
                        end;

                      if (FLossless.lossless_undoscanline(ppixelarray(local_pscanline), ppixelarray(local_cscanline), width, n_c) <> 0) then
                        begin
                          DoStatus('*** Premature EOF: expected %d rows, got %d', [height, n - 1]);
                          found_EOF := 1;
                          Break;
                        end;

                      if (FImageInfo.Components > 1) then
                        begin
                          for my_i := 0 to pred(width + LEFTMARGIN + RIGHTMARGIN) do
                              ppixelarray(cscanline)^[-FImageInfo.Components + my_i * FImageInfo.Components + n_c] := ppixelarray(local_cscanline)^[-1 + my_i];
                        end;
                    end;

                  PTR := @(ppixelarray(cscanline)^[FImageInfo.Components]);
                  write_one_line(PTR, FImageInfo.Components * width, FOutputStream);

                  tot_out := tot_out + FImageInfo.Components * width;

                  { extend the edges }
                  for n_c := 0 to pred(FImageInfo.Components) do
                      ppixelarray(cscanline)^[FImageInfo.Components * (width + 1) + n_c] := ppixelarray(cscanline)^[FImageInfo.Components * width + n_c];

                  { make the current scanline the previous one }
                  swaplines();

                  { Take out the Restart Markers }
                  if (IsTrue(got_restart)) then
                    begin
                      { Look for restart markers only after a restart interval }
                      if ((MCUs_counted mod restart_interval) = 0) then
                        begin
                          FBitIO.bitiflush();
                          FJpeg.read_n_bytes(FInputStream, 2); { read the RST marker }
                          FBitIO.bitiinit();
                        end;
                      inc(MCUs_counted);

                    end;
                  inc(n);
                end; { End of while loop for each file line }
            end
          else
            begin

              { LOSSY MODE }
              inc(n);
              while (n <= height) do
                begin
                  { 'extend' the edges }
                  for n_c := 0 to pred(FImageInfo.Components) do
                    begin
                      ppixelarray(cscanline)^[-FImageInfo.Components + n_c] := ppixelarray(pscanline)^[FImageInfo.Components + n_c];
                      ppixelarray(cscanline)^[n_c] := ppixelarray(pscanline)^[FImageInfo.Components + n_c];
                    end;

                  for n_c := 0 to pred(FImageInfo.Components) do
                    begin
                      if (FImageInfo.Components > 1) then
                        begin
                          for my_i := 0 to pred(width + LEFTMARGIN + RIGHTMARGIN) do
                            begin
                              ppixelarray(local_cscanline)^[-1 + my_i] := ppixelarray(cscanline)^[-FImageInfo.Components + my_i * FImageInfo.Components + n_c];
                              ppixelarray(local_pscanline)^[-1 + my_i] := ppixelarray(pscanline)^[-FImageInfo.Components + my_i * FImageInfo.Components + n_c];
                            end;
                        end
                      else
                        begin
                          local_cscanline := cscanline;
                          local_pscanline := pscanline;
                        end;

                      if (FLossy.lossy_undoscanline(ppixelarray(local_pscanline), ppixelarray(local_cscanline), width, n_c) <> 0) then
                        begin
                          DoStatus('*** Premature EOF: expected %d rows, got %d', [width, n - 1]);
                          found_EOF := 1;
                          Break;
                        end;

                      if (FImageInfo.Components > 1) then
                        begin
                          for my_i := 0 to pred(width + LEFTMARGIN + RIGHTMARGIN) do
                              ppixelarray(cscanline)^[-FImageInfo.Components + my_i * FImageInfo.Components + n_c] := ppixelarray(local_cscanline)^[-1 + my_i];
                        end;
                    end;

                  PTR := @(ppixelarray(cscanline)^[FImageInfo.Components]);
                  write_one_line(PTR, FImageInfo.Components * width, FOutputStream);

                  tot_out := tot_out + FImageInfo.Components * width;

                  { extend the edges }
                  for n_c := 0 to pred(FImageInfo.Components) do
                      ppixelarray(cscanline)^[FImageInfo.Components * (width + 1) + n_c] := ppixelarray(cscanline)^[FImageInfo.Components * width + n_c];

                  { make the current scanline the previous one }
                  swaplines();

                  { Take out the Restart Markers }
                  if (IsTrue(got_restart)) then
                    begin
                      { Look for restart markers only after a restart interval }
                      if ((MCUs_counted mod restart_interval) = 0) then
                        begin
                          FBitIO.bitiflush();
                          FJpeg.read_n_bytes(FInputStream, 2); { read the RST marker }
                          FBitIO.bitiinit();
                        end;
                      inc(MCUs_counted);

                    end;

                end; { End of while loop for each file line }

            end; { End of part for LINE_INT }

        end

      else
        begin { Non LINE_INT mode }
          if (color_mode = PIXEL_INT) then
            begin
              { ***********************************************************************/
                /*           Pixel interleaved mode with single file received          */
                /*********************************************************************** }

              if (lossy = False) then
                begin

                  { LOSSLESS MODE }
                  inc(n);
                  while (n <= height) do
                    begin
                      { 'extend' the edges }
                      for n_c := 0 to pred(FImageInfo.Components) do
                        begin
                          ppixelarray(cscanline)^[-FImageInfo.Components + n_c] := ppixelarray(pscanline)^[FImageInfo.Components + n_c];
                          ppixelarray(cscanline)^[n_c] := ppixelarray(pscanline)^[FImageInfo.Components + n_c];
                        end;

                      if (FLossless.lossless_undoscanline_pixel(ppixelarray(pscanline), ppixelarray(cscanline), FImageInfo.Components * width) <> 0) then
                        begin
                          DoStatus('*** Premature EOF: expected %d rows, got %d', [width, n - 1]);
                          found_EOF := 1;
                          Break;
                        end;

                      PTR := @(ppixelarray(cscanline)^[FImageInfo.Components]);
                      write_one_line(PTR, FImageInfo.Components * width, FOutputStream);

                      tot_out := tot_out + FImageInfo.Components * width;

                      { extend the edges }
                      for n_c := 0 to pred(FImageInfo.Components) do
                          ppixelarray(cscanline)^[FImageInfo.Components * (width + 1) + n_c] := ppixelarray(cscanline)^[FImageInfo.Components * width + n_c];

                      { make the current scanline the previous one }
                      swaplines();

                      { Take out the Restart Markers }
                      if (IsTrue(got_restart)) then
                        begin
                          { Look for restart markers only after a restart interval }
                          if ((MCUs_counted mod restart_interval) = 0) then
                            begin
                              FBitIO.bitiflush();
                              FJpeg.read_n_bytes(FInputStream, 2); { read the RST marker }
                              FBitIO.bitiinit();
                            end;
                          inc(MCUs_counted);

                        end;
                      inc(n);

                    end; { End of line loop for PIXEL_INT }

                end

              else
                begin
                  { LOSSY MODE }
                  inc(n);
                  while (n <= height) do
                    begin
                      { 'extend' the edges }
                      for n_c := 0 to pred(FImageInfo.Components) do
                        begin
                          ppixelarray(cscanline)^[-FImageInfo.Components + n_c] := ppixelarray(pscanline)^[FImageInfo.Components + n_c];
                          ppixelarray(cscanline)^[n_c] := ppixelarray(pscanline)^[FImageInfo.Components + n_c];
                        end;

                      if (FLossy.lossy_undoscanline_pixel(ppixelarray(pscanline), ppixelarray(cscanline), FImageInfo.Components * width) <> 0) then
                        begin
                          DoStatus('*** Premature EOF: expected %d rows, got %d', [width, n - 1]);
                          found_EOF := 1;
                          Break;
                        end;

                      PTR := @(ppixelarray(cscanline)^[FImageInfo.Components]);
                      write_one_line(PTR, FImageInfo.Components * width, FOutputStream);

                      tot_out := tot_out + FImageInfo.Components * width;

                      { extend the edges }
                      for n_c := 0 to pred(FImageInfo.Components) do
                          ppixelarray(cscanline)^[FImageInfo.Components * (width + 1) + n_c] := ppixelarray(cscanline)^[FImageInfo.Components * width + n_c];

                      { make the current scanline the previous one }
                      swaplines();

                      { Take out the Restart Markers }
                      if (IsTrue(got_restart)) then
                        begin
                          { Look for restart markers only after a restart interval }
                          if ((MCUs_counted mod restart_interval) = 0) then
                            begin
                              FBitIO.bitiflush();
                              FJpeg.read_n_bytes(FInputStream, 2); { read the RST marker }
                              FBitIO.bitiinit();
                            end;
                          inc(MCUs_counted);

                        end;
                      inc(n);

                    end; { End of line loop for PIXEL_INT }

                end;

            end
          else
            begin

              { *********************************************************************** }
              { *           Plane interleaved mode                * }
              { *********************************************************************** }

              if (lossy = False) then
                begin
                  { LOSSLESS MODE }
                  inc(n);
                  while (n <= height) do
                    begin
                      { 'extend' the edges }
                      ppixelarray(cscanline)^[-1] := ppixelarray(pscanline)^[1];
                      ppixelarray(cscanline)^[0] := ppixelarray(pscanline)^[1];

                      if (FLossless.lossless_undoscanline(ppixelarray(pscanline), ppixelarray(cscanline), width, n_s) <> 0) then
                        begin
                          DoStatus('*** Premature EOF: expected %d rows, got %d', [height, n - 1]);
                          found_EOF := 1;
                          Break;
                        end;
                      PTR := @(ppixelarray(cscanline)^[1]);

                      write_one_line(PTR, width, FOutputStream);
                      tot_out := tot_out + width;

                      { extend the edges }
                      ppixelarray(cscanline)^[width + 1] := ppixelarray(cscanline)^[width];

                      { make the current scanline the previous one }
                      swaplines;

                      { Take out the Restart Markers }
                      if IsTrue(got_restart) then
                        begin
                          { Look for restart markers only after a restart interval }
                          if ((MCUs_counted mod restart_interval) = 0) then
                            begin
                              FBitIO.bitiflush();
                              FJpeg.read_n_bytes(FInputStream, 2); { read the RST marker }
                              FBitIO.bitiinit();
                            end;
                          inc(MCUs_counted);

                        end;
                      inc(n);
                    end; { End of line loop in PLANE_INT }
                end
              else
                begin

                  { LOSSY MODE }
                  inc(n);
                  while (n <= height) do

                    begin
                      { 'extend' the edges }
                      ppixelarray(cscanline)^[-1] := ppixelarray(pscanline)^[1];
                      ppixelarray(cscanline)^[0] := ppixelarray(pscanline)^[1];

                      if (FLossy.lossy_undoscanline(ppixelarray(pscanline), ppixelarray(cscanline), width, n_s) <> 0) then
                        begin
                          DoStatus('*** Premature EOF: expected %d rows, got %d', [height, n - 1]);
                          found_EOF := 1;
                          Break;
                        end;

                      PTR := @(ppixelarray(cscanline)^[1]);

                      write_one_line(PTR, width, FOutputStream);
                      tot_out := tot_out + width;
                      { extend the edges }
                      ppixelarray(cscanline)^[width + 1] := ppixelarray(cscanline)^[width];

                      { make the current scanline the previous one }
                      swaplines;

                      { Take out the Restart Markers }
                      if IsTrue(got_restart) then
                        begin
                          { Look for restart markers only after a restart interval }
                          if ((MCUs_counted mod restart_interval) = 0) then
                            begin
                              FBitIO.bitiflush();
                              FJpeg.read_n_bytes(FInputStream, 2); { read the RST marker }
                              FBitIO.bitiinit();
                            end;
                          inc(MCUs_counted);

                        end;

                      inc(n);
                    end; { End of line loop in PLANE_INT }
                end;     { End of each component for PLANE_INT }

            end; { End of non LINE_INT }

        end; { End of loop for scans }

      FBitIO.bitiflush();
      FOutputStream.Position := 0;
      mk := 0;

      { Read EOI }
      seek_return := FJpeg.seek_marker(FInputStream, @mk);
      if (seek_return = BUF_EOF) then
        begin
          DoStatus('Did not get EOI at end of compressed image');
          Result := False;
          Exit;
        end;

      if (seek_return > 2) then
        begin
          DoStatus('*** WARNING: %d extra bytes between end of scan and next marker.', [seek_return - 2]);
          DoStatus('***          Added to marker segment count.');
          Result := False;
          Exit;
        end;

      pos0 := pos0 + seek_return;
      if (mk <> JPEGLS_MARKER_EOI) and (mk <> JPEGLS_MARKER_EOI2) then
        begin
          DoStatus('In this implementation last marker must be EOI');
          Result := False;
          Exit;
        end;

      if IsTrue(got_restart) then
          DoStatus('Restart markers were found with a restart interval of %i', [restart_interval]);

      if FEnableLog then
          DoStatus('Marker segment bytes: %d', [pos0]);

      { position in input file }
      pos1 := FBitIO.ftell(FInputStream);

      { size of compressed file read (bits), incl. headers. }
      tot_in := 8 * pos1;

      { Close down }
      FMelcode.close_process_run();
      closebuffers;

      FreeMem(head_frame);

      for mk := 0 to pred(MAX_SCANS) do
          FreeMem(head_scan[mk]);

      if local_scanl0 <> nil then
          FreeMem(local_scanl0);
      if local_scanl1 <> nil then
          FreeMem(local_scanl1);

      if IsTrue(found_EOF) then
          Result := False
      else
          Result := True;
    end;
end;

end. 
 
 
 
