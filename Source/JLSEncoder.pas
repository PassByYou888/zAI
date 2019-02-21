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
unit JLSEncoder;

{$INCLUDE zDefine.inc}

interface

uses
  JLSGlobal, CoreClasses, JLSBitIO, StrUtils, SysUtils, JLSJpegmark, JLSMelcode, JLSLossless, JLSBaseCodec;

const
  LESS_CONTEXTS = 1;

type
  TJLSEncoder = class(TJLSBaseCodec)
  private
    FOwnStreams: Boolean;
    application_header: Int; { application bytes written in the header }
    all_header: Int;         { all bytes of the header, including application bytes and JPEG-LS bytes }
    Shift: Int;              { Shift value for sparse images }
    palete: Int;             { for paletized images }
    { close the line buffers }
    function closebuffers: Int;
    { Initialize the buffers for each line }
    procedure initbuffers(comp: Int);
    { Initialization Function - Reads in parameters from image }
    procedure Initialize;
    { Read one row of pixel values }
    procedure read_one_line(Line: ppixel; Cols: Int; infile: TCoreClassStream);
    { Swap the pointers to the current and previous scanlines }
    procedure swaplines;

    procedure Init;

  public
    constructor Create; override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    procedure SaveToFile(aFileName: string);

  end;

implementation

uses DoStatusIO;

procedure TJLSEncoder.read_one_line(Line: ppixel; Cols: Int; infile: TCoreClassStream);
var
  line8: PByte;
  i: Int;
begin
  if (FImageInfo.bpp16 = False) then
    begin
      line8 := PByte(safealloc(Cols));

      if infile.read(line8^, Cols) <> Cols then
          error('Input file is truncated');

      for i := 0 to pred(Cols) do
        begin
          Line^ := PByteArrayAccess(line8)^[i];
          inc(Line);
        end;
      FreeMem(line8);
    end
  else
    begin
      if infile.read(Line, Cols * 2) <> Cols * 2 then
          error('Input file is truncated');
    end;
end;

procedure TJLSEncoder.SaveToFile(aFileName: string);
var
  stream: TCoreClassStream;
begin
  stream := TCoreClassFileStream.Create(aFileName, fmCreate);
  try
    FOutputStream.Position := 0;
    stream.CopyFrom(FOutputStream, FOutputStream.Size);
  finally
      DisposeObject(stream);
  end;
end;

procedure TJLSEncoder.initbuffers(comp: Int);
var
  PTR: ppixel;
begin
  pscanl0 := safecalloc(comp * (width + LEFTMARGIN + RIGHTMARGIN + NEGBUFFSIZE), SizeOf(Pixel));
  cscanl0 := safecalloc(comp * (width + LEFTMARGIN + RIGHTMARGIN + NEGBUFFSIZE), SizeOf(Pixel));

  { Adjust scan line pointers taking into account the margins,
    and also the fact that indexing for scan lines starts from 1
  }
  PTR := pscanl0;
  inc(PTR, comp * (LEFTMARGIN - 1));
  pscanline := PTR;
  PTR := cscanl0;
  inc(PTR, comp * (LEFTMARGIN - 1));
  cscanline := PTR;

  FBitIO.bitoinit();
end;

procedure TJLSEncoder.swaplines;
var
  Temp: ppixel;
begin
  Temp := pscanline;
  pscanline := cscanline;
  cscanline := Temp;
end;

constructor TJLSEncoder.Create;
begin
  FOwnStreams := False;
  inherited Create;
  Shift := 0;  { Shift value for sparse images }
  palete := 0; { for paletized images }
  Init;
end;

destructor TJLSEncoder.Destroy;
begin

  if FOwnStreams then
    begin
      FInputStream.Free;
      FOutputStream.Free;
    end;

  inherited;
end;

function TJLSEncoder.closebuffers: Int;
var
  Pos: Int;
begin
  FBitIO.bitoflush();

  FBitIO.fclose(FInputStream);

  Pos := FBitIO.ftell(FOutputStream);

  FBitIO.fclose(FOutputStream);

  FreeMem(pscanl0);
  FreeMem(cscanl0);

  Result := Pos;
end;

procedure TJLSEncoder.Initialize;
var
  color_mode_string: string;
  i: Int;
begin
  { check that color mode is valid and pick color mode string }
  if (FImageInfo._near = 0) then
      lossy := False
  else
      lossy := True;

  case color_mode of
    PLANE_INT: color_mode_string := plane_int_string;
    LINE_INT: color_mode_string := line_int_string;
    PIXEL_INT: color_mode_string := pixel_int_string;
    else
      begin
      end;
  end;

  { Single component => PLANE_INT }
  if (((color_mode = LINE_INT) or (color_mode = PIXEL_INT)) and (FImageInfo.Components = 1)) then
    begin
      if (FEnableLog) then
          DoStatus('Single component received: Color mode changed to PLANE INTERLEAVED');
      color_mode := PLANE_INT;
      color_mode_string := plane_int_string;
    end;

  FImageInfo.alpha := alpha0 + 1; { number read from file header is alpha-1 }
  FImageInfo.ceil_half_alpha := (FImageInfo.alpha + 1) div 2;

  FImageInfo.highmask := FImageInfo.highmask - FImageInfo.alpha;
  { check that alpha is a power of 2 }
  alpha0 := FImageInfo.alpha;
  i := -1;
  while IsTrue(alpha0) do
    begin
      alpha0 := shr_c(alpha0, 1);
      inc(i)
    end;

  if (FImageInfo.alpha <> (1 shl i)) then
    begin
      DoStatus('Sorry, this version has been optimized for alphabet size = power of 2, got %d', [FImageInfo.alpha]);
    end;

  { Check for 16 or 8 bit mode }
  if (FImageInfo.alpha <= MAXA16) and (FImageInfo.alpha > MAXA8) then
    begin
      FImageInfo.bpp16 := True;
      lutmax := LUTMAX16;
    end
  else if (FImageInfo.alpha <= MAXA8) and (FImageInfo.alpha >= 1) then
    begin
      FImageInfo.bpp16 := False;
      lutmax := LUTMAX8;
    end
  else begin
      // fprintf(stderr,"Got alpha = %d\n",alpha);
      // error("Bad value for alpha. Sorry...\n");
    end;

  { print out parameters }
  if (FEnableLog) then
    begin
      DoStatus('Image: cols=%d rows=%d alpha=%d comp=%d mode=%d (%s)', [width, height, alpha, Components, color_mode, color_mode_string]);
    end;

  { compute auxiliary parameters for _near-lossless (globals) }
  if (lossy = True) then
    begin
      quant := 2 * FImageInfo._near + 1;
      FImageInfo.qbeta := (FImageInfo.alpha + 2 * FImageInfo._near + quant - 1) div quant;
      FImageInfo.beta := quant * FImageInfo.qbeta;
      ceil_half_qbeta := (FImageInfo.qbeta + 1) div 2;
      FImageInfo.negNEAR := -FImageInfo._near;
      if (FEnableLog) then
          DoStatus('_near-lossless mode: _near = %d  beta = %d  qbeta = %d', [FImageInfo._near, FImageInfo.beta, FImageInfo.qbeta]);
    end;

  { compute bits per sample for input symbols }
  bpp := 1;
  while longint(1 shl bpp) < FImageInfo.alpha do
      inc(bpp);

  { check if alpha is a power of 2: }
  if (FImageInfo.alpha <> (1 shl bpp)) then
      need_lse := 1; { if not, MAXVAL will be non-default, and we'll need to specify it in an LSE marker }

  { compute bits per sample for unencoded prediction errors }
  FImageInfo.qbpp := 1;
  if (lossy = True) then
    while longint(1 shl FImageInfo.qbpp) < FImageInfo.qbeta do
        inc(FImageInfo.qbpp)
  else
      FImageInfo.qbpp := bpp;

  if (bpp < 2) then
      bpp := 2;

  { limit for unary part of Golomb code }
  if (bpp < 8) then
      FImageInfo.Limit := 2 * (bpp + 8) - FImageInfo.qbpp - 1
  else
      FImageInfo.Limit := 4 * bpp - FImageInfo.qbpp - 1;

  for i := 0 to pred(FImageInfo.Components) do
    begin
      samplingx[i] := 1;
      samplingy[i] := 1;
    end;

  { Allocate memory pools. }
  initbuffers(FImageInfo.Components);
end;

function TJLSEncoder.Execute: Boolean;
var
  n, n_c, n_r, my_i, n_s, i: Int;
  tot_in, tot_out, pos0, pos1: long;
  temp_columns: Int;
  MCUs_counted: Int;
  local_scanl0, local_scanl1, local_pscanline, local_cscanline: ppixel;
  PTR: ppixel;
begin
  inherited Execute;
  tot_in := 0;
  tot_out := 0;

  application_header := 0;
  all_header := 0;
  local_scanl0 := nil;
  local_scanl1 := nil;
  { Parse the parameters, initialize }
  Initialize;

  { Compute the number of scans }
  { Multiple scans only for PLANE_INT in this implementation }

  if (color_mode = PLANE_INT) then
      number_of_scans := FImageInfo.Components
  else
      number_of_scans := 1;

  { Write the frame header - allocate memory for jpegls header }
  head_frame := safecalloc(1, SizeOf(tjpeg_ls_header));

  for n_s := 0 to pred(number_of_scans) do
      head_scan[n_s] := safecalloc(1, SizeOf(tjpeg_ls_header));

  { Assigns columns/rows to head_frame }
  head_frame^.columns := width;
  head_frame^.Rows := height;

  head_frame^.alp := FImageInfo.alpha;
  head_frame^.comp := FImageInfo.Components;

  { Assign component id and samplingx/samplingy }
  for i := 0 to pred(FImageInfo.Components) do
    begin
      head_frame^.comp_ids[i] := i + 1;
      head_frame^.samplingx[i] := samplingx[i];
      head_frame^.samplingy[i] := samplingy[i];
    end;

  head_frame^._near := FImageInfo._near; { Not needed, scan information }
  head_frame^.need_lse := need_lse;      { Not needed, for commpletness }
  head_frame^.color_mode := color_mode;  { Not needed, scan information }
  head_frame^.Shift := Shift;            { Not needed, scan information }

  for n_s := 0 to pred(number_of_scans) do
    begin
      head_scan[n_s]^.alp := FImageInfo.alpha;
      head_scan[n_s]^._near := FImageInfo._near;
      head_scan[n_s]^.t1 := FT1;
      head_scan[n_s]^.t2 := FT2;
      head_scan[n_s]^.t3 := FT3;
      head_scan[n_s]^.res := FImageInfo.Reset;
      head_scan[n_s]^.Shift := Shift;
      head_scan[n_s]^.color_mode := color_mode;
    end;

  if (color_mode = PLANE_INT) then { One plane per scan }
    begin
      for n_s := 0 to pred(number_of_scans) do
        begin
          head_scan[n_s]^.comp := 1;
          head_scan[n_s]^.comp_ids[0] := n_s + 1;
        end;
    end
  else begin
      for n_s := 0 to pred(number_of_scans) do
        begin
          head_scan[n_s]^.comp := head_frame^.comp;
          for n_c := 0 to pred(head_frame^.comp) do
              head_scan[n_s]^.comp_ids[n_c] := n_c + 1;
        end;
    end;

  { Write SOI }
  all_header := FJpeg.write_marker(FOutputStream, JPEGLS_MARKER_SOI2);

  { Write the frame }
  all_header := all_header + FJpeg.write_jpegls_frame(FOutputStream, head_frame);

  { End of frame header writing }

  if ((FImageInfo.Components > 1)) then
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

  { Go through each scan and process line by line }
  for n_s := 0 to pred(number_of_scans) do
    begin

      { process scans one by one }

      if (n_s = 0) then
        begin
          { The thresholds for the scan. Must re-do per scan is change. }
          set_thresholds(FImageInfo.alpha, FImageInfo._near, @FT1, @FT2, @FT3);
          for i := 0 to pred(number_of_scans) do
            begin
              head_scan[n_s]^.t1 := FT1;
              head_scan[n_s]^.t2 := FT2;
              head_scan[n_s]^.t3 := FT3;
            end;
        end;

      { After the thresholds are set, write LSE marker if we have }
      { non-default parameters or if we need a mapping table }
      if (need_lse <> 0) then
          all_header := all_header + FJpeg.write_jpegls_extmarker(FOutputStream, head_scan[n_s], LSE_PARAMS);

      { If using restart markers, write the DRI header }
      if (need_restart <> 0) then
        begin
          head_scan[n_s]^.restart_interval := restart_interval;
          all_header := all_header + FJpeg.write_jpegls_restartmarker(FOutputStream, head_scan[n_s]);
        end;

      { Print out parameters }
      if FEnableLog then
          DoStatus('Parameters: T1=%d T2=%d T3=%d RESET=%d limit=%d', [t1, t2, t3, Reset, Limit]);

      { Prepare LUTs for context quantization }
      { Must re-do when Thresholds change }
      prepareLUTs();

      if (lossy = True) then { prepare div/mul tables for _near-lossless quantization }
          prepare_qtables(FImageInfo.alpha, FImageInfo._near);

      { Check for errors }
      check_compatibility(head_frame, head_scan[0], 0);

      { Restart Marker is reset after every scan }
      MCUs_counted := 0;

      { Write the scan header }
      all_header := all_header + FJpeg.write_jpegls_scan(FOutputStream, head_scan[n_s]);
      pos0 := FBitIO.ftell(FOutputStream); { position in output file, after header }

      { Initializations for each scan }
      { Start from 1st image row }
      n := 0;

      { initialize stats arrays }
      if (lossy = True) then
          init_stats(FImageInfo.qbeta)
      else
          init_stats(FImageInfo.alpha);

      { initialize run processing }
      FMelcode.init_process_run;

      if (color_mode = LINE_INT) then { line interleaved }
        begin
          { ***********************************************************************/
            /*           Line interleaved mode with single file received           */
            /*********************************************************************** }

          if (lossy = False) then
            begin
              inc(n);
              { LOSSLESS mode }
              while (n <= height) do
                begin
                  PTR := @(pwordarray(cscanline)^[FImageInfo.Components + 1]);
                  read_one_line(PTR, FImageInfo.Components * width, FInputStream);
                  tot_in := tot_in + FImageInfo.Components * width;

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
                      else begin
                          local_cscanline := cscanline;
                          local_pscanline := pscanline;
                        end;

                      { process the lines }
                      FLossless.lossless_doscanline(ppixelarray(local_pscanline), ppixelarray(local_cscanline), width, n_c);
                    end;

                  { 'extend' the edges }
                  for n_c := 0 to pred(FImageInfo.Components) do
                      ppixelarray(cscanline)^[FImageInfo.Components * (width + 1) + n_c] := ppixelarray(cscanline)^[FImageInfo.Components * width + n_c];

                  { make the current scanline the previous one }
                  swaplines();

                  { Insert restart markers if enabled }
                  if IsTrue(need_restart) then
                    begin
                      { Insert restart markers only after a restart interval }
                      if ((MCUs_counted mod restart_interval) = 0) then
                        begin
                          FBitIO.bitoflush();
                          FJpeg.write_marker(FOutputStream, (JPEGLS_MARKER_RSTm + ((MCUs_counted div restart_interval) mod 8)));
                        end;
                      inc(MCUs_counted);
                    end;
                  inc(n);
                end;

            end
          else
            begin
              inc(n);
              { LOSSY mode }
              while (n <= height) do
                begin
                  PTR := @(pwordarray(cscanline)^[FImageInfo.Components + 1]);

                  read_one_line(PTR, FImageInfo.Components * width, FInputStream);

                  tot_in := tot_in + FImageInfo.Components * width;

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
                      else begin
                          local_cscanline := cscanline;
                          local_pscanline := pscanline;
                        end;

                      { process the lines }
                      FLossy.lossy_doscanline(ppixelarray(local_pscanline), ppixelarray(local_cscanline), width, n_c);

                      if (Components > 1) then
                        begin
                          for my_i := 0 to pred(width + LEFTMARGIN + RIGHTMARGIN) do
                            begin
                              ppixelarray(cscanline)^[-Components + my_i * Components + n_c] := ppixelarray(local_cscanline)^[-1 + my_i];
                            end;
                        end;
                    end;

                  { 'extend' the edges }
                  for n_c := 0 to pred(FImageInfo.Components) do
                      ppixelarray(cscanline)^[FImageInfo.Components * (width + 1) + n_c] := ppixelarray(cscanline)^[FImageInfo.Components * width + n_c];

                  { make the current scanline the previous one }
                  swaplines();

                  { Insert restart markers if enabled }
                  if IsTrue(need_restart) then
                    begin
                      { Insert restart markers only after a restart interval }
                      if ((MCUs_counted mod restart_interval) = 0) then
                        begin
                          FBitIO.bitoflush();
                          FJpeg.write_marker(FOutputStream, (JPEGLS_MARKER_RSTm + ((MCUs_counted div restart_interval) mod 8)));
                        end;
                      inc(MCUs_counted);
                    end;
                  inc(n);
                end;

            end;
        end { Closes part for color_mode=LINE_INT }
      else
        begin

          if (color_mode = PIXEL_INT) then
            begin
              { ***********************************************************************
                *           Pixel interleaved mode with single file received          *
                *********************************************************************** }

              if (lossy = False) then
                begin
                  inc(n);

                  { LOSSLESS mode }
                  while (n <= height) do
                    begin
                      PTR := @(pwordarray(cscanline)^[FImageInfo.Components + 1]);

                      read_one_line(PTR, FImageInfo.Components * width, FInputStream);

                      tot_in := tot_in + FImageInfo.Components * width;

                      { 'extend' the edges }

                      for n_c := 0 to pred(FImageInfo.Components) do
                        begin
                          ppixelarray(cscanline)^[-FImageInfo.Components + n_c] := ppixelarray(pscanline)^[FImageInfo.Components + n_c];
                          ppixelarray(cscanline)^[n_c] := ppixelarray(pscanline)^[FImageInfo.Components + n_c];
                        end;

                      { process the lines }
                      FLossless.lossless_doscanline_pixel(ppixelarray(pscanline), ppixelarray(cscanline), FImageInfo.Components * width);

                      { 'extend' the edges }
                      for n_c := 0 to pred(FImageInfo.Components) do
                          ppixelarray(cscanline)^[FImageInfo.Components * (width + 1) + n_c] := ppixelarray(cscanline)^[FImageInfo.Components * width + n_c];

                      { make the current scanline the previous one }
                      swaplines();

                      { Insert restart markers if enabled }
                      if IsTrue(need_restart) then
                        begin
                          { Insert restart markers only after a restart interval }
                          if ((MCUs_counted mod restart_interval) = 0) then
                            begin
                              FBitIO.bitoflush();
                              FJpeg.write_marker(FOutputStream, (JPEGLS_MARKER_RSTm + ((MCUs_counted div restart_interval) mod 8)));
                            end;
                          inc(MCUs_counted);
                        end;
                      inc(n);
                    end;
                end
              else
                begin
                  inc(n);

                  { LOSSY mode }
                  while (n <= height) do
                    begin
                      PTR := @(pwordarray(cscanline)^[FImageInfo.Components + 1]);

                      read_one_line(PTR, FImageInfo.Components * width, FInputStream);

                      tot_in := tot_in + FImageInfo.Components * width;

                      { 'extend' the edges }

                      for n_c := 0 to pred(FImageInfo.Components) do
                        begin
                          ppixelarray(cscanline)^[-FImageInfo.Components + n_c] := ppixelarray(pscanline)^[FImageInfo.Components + n_c];
                          ppixelarray(cscanline)^[n_c] := ppixelarray(pscanline)^[FImageInfo.Components + n_c];
                        end;

                      { process the lines }
                      FLossy.lossy_doscanline_pixel(ppixelarray(pscanline), ppixelarray(cscanline), FImageInfo.Components * width);

                      { 'extend' the edges }
                      for n_c := 0 to pred(FImageInfo.Components) do
                          ppixelarray(cscanline)^[FImageInfo.Components * (width + 1) + n_c] := ppixelarray(cscanline)^[FImageInfo.Components * width + n_c];

                      { make the current scanline the previous one }
                      swaplines();

                      { Insert restart markers if enabled }
                      if IsTrue(need_restart) then
                        begin
                          { Insert restart markers only after a restart interval }
                          if ((MCUs_counted mod restart_interval) = 0) then
                            begin
                              FBitIO.bitoflush();
                              FJpeg.write_marker(FOutputStream, (JPEGLS_MARKER_RSTm + ((MCUs_counted div restart_interval) mod 8)));
                            end;
                          inc(MCUs_counted);
                        end;
                      inc(n);
                    end;

                end;

            end { Closes if PIXEL_INT }
          else
            begin { NON PIXEL_INT }

              { ***********************************************************************/
                /*           Plane interleaved mode                    */
                /*********************************************************************** }

              if (lossy = False) then
                begin
                  { LOSSLESS mode }
                  inc(n);

                  while (n <= height) do
                    begin

                      temp_columns := width;;

                      PTR := @(pwordarray(cscanline)^[1]);

                      read_one_line(PTR, temp_columns, InputStream);

                      tot_in := tot_in + temp_columns;

                      { 'extend' the edges }
                      ppixelarray(cscanline)^[0] := ppixelarray(pscanline)^[1];
                      ppixelarray(cscanline)^[-1] := ppixelarray(pscanline)^[1];

                      { process the lines }
                      FLossless.lossless_doscanline(ppixelarray(pscanline),
                        ppixelarray(cscanline),
                        temp_columns,
                        n_s);

                      { 'extend' the edges }
                      ppixelarray(cscanline)^[temp_columns + 1] := ppixelarray(cscanline)^[temp_columns];

                      { make the current scanline the previous one }
                      swaplines; { c_swaplines(n_s) };

                      { Insert restart markers if enabled }
                      if (IsTrue(need_restart)) then
                        begin
                          { Insert restart markers only after a restart interval }
                          if ((MCUs_counted mod restart_interval) = 0) then
                            begin
                              FBitIO.bitoflush();
                              FJpeg.write_marker(FOutputStream, (JPEGLS_MARKER_RSTm + ((MCUs_counted div restart_interval) mod 8)));
                            end;
                          inc(MCUs_counted);
                        end;
                      inc(n);
                    end; // while

                end // if
              else
                begin

                  { LOSSY mode }
                  inc(n);
                  while (n <= height) do
                    begin

                      temp_columns := width;

                      PTR := @(pwordarray(cscanline)^[1]);

                      read_one_line(PTR, temp_columns, InputStream);

                      tot_in := tot_in + temp_columns;

                      { 'extend' the edges }
                      ppixelarray(cscanline)^[0] := ppixelarray(pscanline)^[1];
                      ppixelarray(cscanline)^[-1] := ppixelarray(pscanline)^[1];

                      { process the lines }
                      FLossy.lossy_doscanline(ppixelarray(pscanline),
                        ppixelarray(cscanline),
                        temp_columns,
                        n_s);

                      { 'extend' the edges }
                      ppixelarray(cscanline)^[temp_columns + 1] := ppixelarray(cscanline)^[temp_columns];

                      { make the current scanline the previous one }
                      swaplines; { c_swaplines(n_s) };

                      { Insert restart markers if enabled }
                      if (IsTrue(need_restart)) then
                        begin
                          { Insert restart markers only after a restart interval }
                          if ((MCUs_counted mod restart_interval) = 0) then
                            begin
                              FBitIO.bitoflush();
                              FJpeg.write_marker(FOutputStream, (JPEGLS_MARKER_RSTm + ((MCUs_counted div restart_interval) mod 8)));
                            end;
                          inc(MCUs_counted);
                        end;
                      inc(n);
                    end; // while

                end; { End for each component in PLANE_INT }
            end;

        end; { End for non LINE_INT }

      FBitIO.bitoflush();

    end; { End of loop on scans }

  all_header := all_header + FJpeg.write_marker(FOutputStream, JPEGLS_MARKER_EOI2);

  { Close down }
  FMelcode.close_process_run();
  pos1 := closebuffers;

  FreeMem(head_frame);
  for n_s := 0 to pred(number_of_scans) do
      FreeMem(head_scan[n_s]);

  if local_scanl0 <> nil then
      FreeMem(local_scanl0);
  if local_scanl1 <> nil then
      FreeMem(local_scanl1);

  { total bytes out, including JPEG-LS header, but not application-specific header bytes }

  tot_out := pos1 * 8;

  if IsTrue(need_restart) then
      DoStatus('Used restart markers with restart interval : %d', [restart_interval]);

  if FEnableLog then
      DoStatus('Marker segment bytes: %d', [all_header]);

  Result := True; { OK! }
end;

procedure TJLSEncoder.Init;
begin
  color_mode := DEFAULT_COLOR_MODE;
  need_lse := 0;
  need_table := 0;
  need_restart := 0;
  restart_interval := 0;
  FImageInfo.Components := 0;
  FT1 := 0;
  FT2 := 0;
  FT3 := 0;

  FImageInfo.Reset := DEFAULT_RESET;

  { Initialize _near to zero and loss-less mode }
  FImageInfo._near := DEF_NEAR;
  lossy := False;
  alpha0 := DEF_ALPHA;
end;

end.
