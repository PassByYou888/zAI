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
unit JLSGlobal;

{$INCLUDE zDefine.inc}

interface

uses
  CoreClasses, PascalStrings, ListEngine;

const
  { Version number }
  JPEGLSVERSION = 'V.2.3'; // last by 600585@qq.com

  { Maximal number of components in the implementation }
  MAX_COMPONENTS = 6;
  MAX_SCANS = MAX_COMPONENTS;

  { For 1st component of plane interl. mode }
  First = 1;

  { Different colour modes }
  PLANE_INT = 0;
  LINE_INT = 1;
  PIXEL_INT = 2;

  DEFAULT_COLOR_MODE = LINE_INT;

  { margins for scan lines }
  LEFTMARGIN = 2;
  RIGHTMARGIN = 2;

  { alphabet size }
  MAXA8 = 256;
  MAXA16 = 65536;
  LUTMAX8 = 256;
  LUTMAX16 = 4501;

  DEF_NEAR = 0;
  DEF_ALPHA = 255;

  { Quantization threshold basic defaults
    These are the defaults for LOSSLESS, 8 bpp. Defaults for other
    cases are derived from these basic values
  }
  BASIC_T1 = 3;
  BASIC_T2 = 7;
  BASIC_T3 = 21;
  BASIC_Ta = 5;

  CREGIONS = 9; { quantization regions for d-b, b-c, c-a }

  MAXRUN = 64;
  EOLINE = 1;
  NOEOLINE = 0;

  { number of different contexts }
  CONTEXTS1 = (CREGIONS * CREGIONS * CREGIONS);

  CONTEXTS = ((CONTEXTS1 + 1) div 2); { all regions, with symmetric merging }

  MAX_C = 127;
  MIN_C = -128;


  // MAXCODE= (N_R_L_ERROR);

  { Number of end-of-run contexts }
  EOR_CONTEXTS = 2;

  { Total number of contexts }
  TOT_CONTEXTS = (CONTEXTS + EOR_CONTEXTS);

  { index of first end-of-run context }
  EOR_0 = CONTEXTS;

  { index of run state }
  RUNSTATE = 0;

  { *** offsets }

  { The longest code the bit IO can facilitate }
  MAXCODELEN = 24;

  { The stat initialization values }
  INITNSTAT = 1;      { init value for N[] }
  MIN_INITABSTAT = 2; { min init value for A[] }
  INITABSLACK = 6; { init value for A is roughly 2^(bpp-INITABSLACK) but not less than above }
  INITBIASTAT = 0; { init value for B[] }

  { reset values }
  DEFAULT_RESET = 64;
  MINRESET = 3;

  BUF_EOF = -1;

  { define color mode strings }
  plane_int_string = 'plane by plane';
  line_int_string = 'line intlv';
  pixel_int_string = 'sample intlv';

  NEGBUFFSIZE = 4;

type
  Pixel = Word;
  ppixel = ^Pixel;
  Int = Integer;
  pint = ^Int;

  TByteArray = array [0 .. MaxInt div SizeOf(Byte) - 1] of Byte;
  PByteArray = ^TByteArray;

  TWordArray = array [0 .. MaxInt div SizeOf(Word) - 1] of Word;
  pwordarray = ^TWordArray;

  IntArrayAccess = array [0 .. MaxInt div SizeOf(Int) - 1] of Int;
  ByteArrayAccess = array [0 .. MaxInt - 1] of Byte;
  PixelArray = array [-NEGBUFFSIZE .. MaxInt div SizeOf(Word) - (NEGBUFFSIZE + 1)] of Pixel;

  PIntArrayAccess = ^IntArrayAccess;
  PByteArrayAccess = ^ByteArrayAccess;
  ppixelarray = ^PixelArray;
  size_t = Cardinal;
  uint = Cardinal;
  short = SmallInt;
  long = longint;
  ulong = nativeUInt;
  TBytes = array of Byte;

  TABLE_ARRAY = array [0 .. MAX_COMPONENTS - 1] of pwordarray;
  PTABLE = ^TABLE_ARRAY;

  /// <summary>
  /// Defines the JPEG-LS preset coding parameters as defined in ISO/IEC 14495-1, C.2.4.1.1.
  /// JPEG-LS defines a default set of parameters, but custom parameters can be used.
  /// When used these parameters are written into the encoded bit stream as they are needed for the decoding process.
  /// </summary>
  TJlsCustomParameters = record
    /// <summary>
    /// Maximum possible value for any image sample in a scan.
    /// This must be greater than or equal to the actual maximum value for the components in a scan.
    /// </summary>
    MaxVal: Integer;
    /// <summary>
    /// First quantization threshold value for the local gradients.
    /// </summary>
    t1: Integer;
    /// <summary>
    /// Second quantization threshold value for the local gradients.
    /// </summary>
    t2: Integer;
    /// <summary>
    /// Third quantization threshold value for the local gradients.
    /// </summary>
    t3: Integer;
    /// <summary>
    /// Value at which the counters A, B, and N are halved.
    /// </summary>
    Reset: Integer;
  end;

  PJlsParameters = ^TJlsParameters;

  TJlsParameters = record
    /// <summary>
    /// Width of the image in pixels.
    /// </summary>
    width: Integer;
    /// <summary>
    /// Height of the image in pixels.
    /// </summary>
    height: Integer;
    /// <summary>
    /// The number of valid bits per sample to encode.
    /// Valid range 2 - 16. When greater than 8, pixels are assumed to stored as two bytes per sample, otherwise one byte per sample is assumed.
    /// </summary>
    BitsPerSample: Integer;
    /// <summary>
    /// The number of components.
    /// Typical 1 for monochrome images and 3 for color images.
    /// </summary>
    Components: Integer;
    /// <summary>
    /// Defines the allowed lossy error. Value 0 defines lossless.
    /// </summary>
    AllowedLossyError: Integer;
    Custom: TJlsCustomParameters;
  end;

type
  pjpeg_ls_header = ^tjpeg_ls_header;

  tjpeg_ls_header = record
    columns: Integer;                                      { The number of columns }
    Rows: Integer;                                         { Number of rows }
    alp: Integer;                                          { alphabet size (Max+1) , 2 bytes }
    comp: Integer;                                         { number of components, 1 byte }
    _near: Integer;                                        { _near-lossless error, 1 byte }
    color_mode: Integer;                                   { indicates the color mode , 1 byte }
    need_lse: Integer;                                     { Indicates non-default parameters }
    need_table: Integer;                                   { Indicates use of mapping table }
    need_restart: Integer;                                 { Indicates use of restart markers }
    restart_interval: Integer;                             { The number of MCU's between restart markers }
    Shift: Integer;                                        { for sparse images, 1 byte }
    t1, t2, t3: Integer;                                   { Thresholds, 2 bytes each }
    res: Integer;                                          { reset value for counters, 2 bytes }
    samplingx: array [0 .. MAX_COMPONENTS - 1] of Integer; { col. sampling rates 1 byte each }
    samplingy: array [0 .. MAX_COMPONENTS - 1] of Integer; { row sampling rates }
    comp_ids: array [0 .. MAX_COMPONENTS - 1] of Integer;  { component id's }
    acc_size: Integer;                                     { 1 byte }
    AddS: array [0 .. MAX_COMPONENTS - 1] of Integer;      { size given by acc_size }
    TID: uint;                                             { Table ID, 1 byte }
    MAXTAB: uint;                                          { Maximum table index value }
    WT: uint;                                              { Width of each table entry, 1 byte }
    Table: PTABLE;                                         { The table(s) for each component }
  end;

  PImageInfo = ^TImageInfo;

  TImageInfo = record
    bpp16: Boolean; { Indicates if 16 bits per pixel mode or not }
    width: Int;
    height: Int;
    Components: Integer;
    limit_reduce: Integer; { reduction on above for EOR states }
    qbpp: Integer;         { bits per sample for quantized prediction errors }
    alpha: Integer;        { alphabet size }
    Limit: Integer;        { limit for unary part of Golomb code }
    Reset: Integer;
    highmask: Integer;        { for powers of 2, a mask for high bits }
    ceil_half_alpha: Integer; { ceil(alpha/2) }
    _near: Integer;           { loss tolerance }
    negNEAR: Integer;         { LOSSY Mode }

    qdiv0: PIntegerArray; { quantization table (division via look-up) }
    qdiv: PInteger;       { quantization table (division via look-up) }
    qmul0: PIntegerArray; { dequantization table }
    qmul: PInteger;       { dequantization table }

    quant: Int;           { quantization = 2*_near+1 }
    beta: Int;            { size of extended alphabet }
    qbeta: Int;           { size of quantized alphabet }
    ceil_half_qbeta: Int; { ceil(qbeta/2) }
    alpha1eps: Int;       { alpha-1+_near }

    n: array [0 .. TOT_CONTEXTS - 1] of Int;
    a: array [0 .. TOT_CONTEXTS - 1] of Int;
    b: array [0 .. TOT_CONTEXTS - 1] of Int;
    c: array [0 .. TOT_CONTEXTS - 1] of Int;

    vLUT: array [0 .. 3 - 1, 0 .. 2 * LUTMAX16 - 1] of Int;
    classmap: array [0 .. CONTEXTS1 - 1] of Int;
  end;

procedure error(Msg: string);
function safealloc(Size: size_t): Pointer;
function safecalloc(numels, Size: size_t): Pointer;

function predict(rb, RA, RC: Word): Word; inline;

function IsTrue(AValue: Integer): Boolean; inline;
function ENDIAN8(x: Word): Byte; inline;
function ENDIAN16(x: Word): Word; inline;

function check_compatibility(head_frame: pjpeg_ls_header; head_scan: pjpeg_ls_header; n_s: Int): Int;

function shr_c(Value: Int64; ShiftBits: Integer): Int64; overload; inline;
function shr_c(Value: Integer; ShiftBits: Integer): Integer; overload; inline;

function Bool_c(AValue: Boolean): Integer; inline;

implementation

uses DoStatusIO;

function ENDIAN8(x: Word): Byte;
begin
  Result := (x and $000000FF)
end;

function ENDIAN16(x: Word): Word;
begin
  Result := x; // ( ((x shr 8) or(x shl 8)) and $0000ffff);
end;

function Bool_c(AValue: Boolean): Integer;
begin
  if AValue then
      Result := 1
  else
      Result := 0;
end;

function IsTrue(AValue: Integer): Boolean;
begin
  Result := AValue <> 0;
end;

function shr_c(Value: Int64; ShiftBits: Integer): Int64; overload;
begin
  Result := Value shr ShiftBits;
  if (Value and $8000000000000000) > 0 then
      Result := Result or ($FFFFFFFFFFFFFFFF shl (64 - ShiftBits));
end;

function shr_c(Value: Integer; ShiftBits: Integer): Integer; overload;
begin
  Result := Value shr ShiftBits;
  if (Value and $80000000) > 0 then
      Result := Result or Integer($FFFFFFFF shl (64 - ShiftBits));
end;

{ function to print out error messages }
procedure error(Msg: string);
begin
  RaiseInfo(Msg);
end;

{ function to safely call malloc }
function safealloc(Size: size_t): Pointer;
var
  Temp: Pointer;
begin
  Temp := AllocMem(Size);
  if (Temp = nil) then
      error('safealloc: Out of memory. Aborting...');
  Result := Temp;
end;

{ function to safely call calloc }
function safecalloc(numels, Size: size_t): Pointer;
var
  Temp: Pointer;
begin
  Temp := AllocMem(numels * Size);
  if (Temp = nil) then
      error('safecalloc: Out of memory. Aborting...');
  Result := Temp;
end;

{ macro to predict Px }
function predict(rb, RA, RC: Word): Word;
var
  MinX, MaxX: Pixel;
begin
  if (rb > RA) then
    begin
      MinX := RA;
      MaxX := rb;
    end
  else
    begin
      MaxX := RA;
      MinX := rb;
    end;

  if (RC >= MaxX) then
      Result := MinX
  else if (RC <= MinX) then
      Result := MaxX
  else
      Result := RA + rb - RC;
end;

{ We first check compatibility with JPEG-LS, then with this implementation }

function check_compatibility(head_frame: pjpeg_ls_header; head_scan: pjpeg_ls_header; n_s: Int): Int;
var
  number_of_scans, i: Int;
  maxreset: Int;
begin
  Result := 0;
  { Check implemented color modes }
  if ((head_scan^.color_mode > PIXEL_INT)) then
    begin
      DoStatus('Color mode %d not supported.', [head_scan^.color_mode]);
      Result := 10;
      Exit;
    end;

  if (head_scan^.color_mode = PLANE_INT) then
      number_of_scans := head_frame^.comp
  else
      number_of_scans := 1;

  { Test standard compatibility }

  if (head_frame^.columns <= 0) or (head_frame^.Rows <= 0) then
    begin
      DoStatus('Image size must be positive for this implementation.');
      Result := 10;
      Exit;
    end;

  if (head_frame^.alp < 4) then
    begin
      DoStatus('Alphabet size must be >= 4, got %d.', [head_frame^.alp]);
      Result := 10;
      Exit;
    end;

  if (head_scan^.t1 > head_scan^.t2) or (head_scan^.t2 > head_scan^.t3) or
    (head_scan^.t1 < head_scan^._near + 1) or (head_scan^.t3 >= head_scan^.alp) then
    begin
      DoStatus('Bad thresholds: must be %d <= Ta <= Tb <= Tc <= %d.', [head_scan^._near + 1, head_scan^.alp - 1]);
      Result := 10;
      Exit;
    end;

  if (head_frame^.comp > 255) then
    begin
      DoStatus('Too many components (must be less than 255).');
      Result := 10;
      Exit;
    end;

  if (head_scan^._near >= head_scan^.alp) then
    begin
      DoStatus('Error for _near-lossless must be smaller than alphabet (%d), got %d.', [head_scan^.alp, head_scan^._near]);
      Result := 10;
      Exit;
    end;

  if (head_scan^.alp >= 256) then
      maxreset := head_scan^.alp - 1
  else
      maxreset := 255;

  if (head_scan^.res < MINRESET) or (head_scan^.res > maxreset) then
    begin
      DoStatus('Reset parameter must be between %d and %d.', [MINRESET, head_scan^.alp - 1]);
      Result := 10;
      Exit;
    end;

  for i := 0 to pred(head_frame^.comp) do
    if (head_frame^.comp_ids[i] <> (i + 1)) then
      begin
        DoStatus('Components id in frame not compatible with this implementation.');
        Result := 10;
        Exit;
      end;

  if (number_of_scans = 1) then
    begin
      if (head_frame^.comp <> head_scan^.comp) then
        begin
          DoStatus('In this implementation, when single scan, all components must be in the scan.');
          Result := 10;
          Exit;
        end;

      for i := 0 to pred(head_frame^.comp) do
        if (head_scan^.comp_ids[i] <> (i + 1)) then
          begin
            DoStatus('Components id in single scan not compatible with this implementation.');
            Result := 10;
            Exit;
          end;

    end
  else
    begin
      if (head_scan^.comp <> 1) then
        begin
          DoStatus('Only 1 component per scan for plane interleaved mode.');
          Result := 10;
          Exit;
        end;

      if (head_scan^.comp_ids[0] <> (n_s + 1)) then
        begin
          DoStatus('Components id in multiple scan not compatible with this implementation.');
          Result := 10;
          Exit;
        end;

    end;
end;

end.
