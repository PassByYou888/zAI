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
unit JLSBaseCodec;

{$INCLUDE zDefine.inc}

interface

uses
  JLSGlobal, JLSJpegmark, JLSBitIO, JLSMelcode, CoreClasses, PascalStrings, ListEngine,
  JLSLossless, JLSLossy, Math;

type

  { TbsJLSBaseCodec }

  TJLSBaseCodec = class
  private

    function GetComponents: Int;
    function GetHeight: Int;
    function GetWidth: Int;
    procedure SetComponents(AValue: Int);
    procedure SetHeight(AValue: Int);
    procedure SetWidth(AValue: Int);
    function GetRESET: Int;
    procedure SetRESET(const Value: Int);
    function GetNEAR: Int;
    procedure SetNEAR(const Value: Int);
    procedure SetT1(const Value: Int);
    procedure SetT2(const Value: Int);
    procedure SetT3(const Value: Int);
    procedure SetMAXVAL(const Value: Int);
    procedure SetInterleaveMode(const Value: Integer);
    procedure SetRestartInterval(const Value: Integer);
    function GetLimit: Integer;
    procedure SetLimit(const Value: Integer);
    function GetAlpha: Integer;
    procedure SetAlpha(const Value: Integer);
  protected
    FBitIO: TJLSBitIO;
    FJpeg: TJLSJpegMark;
    FMelcode: TJLSMelcode;
    FLossless: TJLSLossless;
    FLossy: TJLSLossy;
    // initialization
    FImageInfo: TImageInfo;
    FInputStream: TCoreClassStream;
    FOutputStream: TCoreClassStream;
    FEnableLog: Boolean;
    pscanline, cscanline: ppixel;
    pscanl0, cscanl0: ppixel;

    head_frame: pjpeg_ls_header;
    head_scan: array [0 .. MAX_SCANS - 1] of pjpeg_ls_header;

    samplingx: array [0 .. MAX_COMPONENTS - 1] of Int;
    samplingy: array [0 .. MAX_COMPONENTS - 1] of Int;

    c_columns: array [0 .. MAX_COMPONENTS - 1] of Int;
    c_rows: array [0 .. MAX_COMPONENTS - 1] of Int;

    whose_max_size_rows, whose_max_size_columns: Int;
    number_of_scans: Int;
    color_mode: Int;
    alpha0: Int;
    restart_interval: Int; { indicates the restart interval }

    // encode...
    need_lse: Int;     { if we need an LSE marker (non-default params) }
    need_table: Int;   { if we need an LSE marker (mapping table) }
    need_restart: Int; { if we need to add restart markers }

    function prepareLUTs: Int;
    function prepare_qtables(absize: Int; AnNEAR: Int): Int;
    procedure init_stats(absize: Int);
    function GetInputStream: TCoreClassStream;
    function GetOutputStream: TCoreClassStream;
    procedure SetInputStream(const Value: TCoreClassStream);
    procedure SetOutputStream(const Value: TCoreClassStream);
    { Set thresholds to default unless specified by header: }
    procedure set_thresholds(alfa, AnNEAR: Int; T1p, T2p, T3p: pint);
  public
    sampling: array [0 .. MAX_COMPONENTS - 1] of Integer;
    disclaimer: PChar;
    ceil_half_qbeta: Integer;

    bpp: Integer; { bits per sample }
    { for LOSSY mode }
    quant: Integer;

    lutmax: Int;
    lossy: Boolean;
    nopause: Boolean; { whether to pause the legal notice or not }
    nolegal: Boolean; { whether to print the legal notice or not }

    { Context quantization thresholds  - initially unset }
    FT3: Int;
    FT2: Int;
    FT1: Int;
    FTa: Int;
    constructor Create; virtual;
    destructor Destroy; override;
    function Execute: Boolean; virtual;
    property height: Int read GetHeight write SetHeight;
    property width: Int read GetWidth write SetWidth;
    property Reset: Int read GetRESET write SetRESET;
    property _near: Int read GetNEAR write SetNEAR;
    property Components: Int read GetComponents write SetComponents;
    property t3: Int read FT3 write SetT3;
    property t2: Int read FT2 write SetT2;
    property t1: Int read FT1 write SetT1;
    property MaxVal: Int read alpha0 write SetMAXVAL;
    property Limit: Integer read GetLimit write SetLimit;
    property alpha: Integer read GetAlpha write SetAlpha;
    property InterleavedMode: Integer read color_mode write SetInterleaveMode;
    property RestartInterval: Integer read restart_interval write SetRestartInterval;
    property InputStream: TCoreClassStream read GetInputStream write SetInputStream;
    property OutputStream: TCoreClassStream read GetOutputStream write SetOutputStream;
    property EnableLog: Boolean read FEnableLog write FEnableLog;

  end;

implementation

{ TbsJLSBaseCoder }

uses DoStatusIO;

constructor TJLSBaseCodec.Create;
begin
  FInputStream := nil;
  FOutputStream := nil;
  FBitIO := TJLSBitIO.Create(FInputStream, FOutputStream);
  FJpeg := TJLSJpegMark.Create(FBitIO, @FImageInfo);
  FMelcode := TJLSMelcode.Create(FBitIO, @FImageInfo);
  FLossless := TJLSLossless.Create(FBitIO, FMelcode, @FImageInfo);
  FLossy := TJLSLossy.Create(FBitIO, FMelcode, @FImageInfo);
  FT3 := -1;
  FT2 := -1;
  FT1 := -1;
  FTa := -1;
  FImageInfo.Reset := 64;
  FEnableLog := False;
  nopause := True;
  nolegal := True;
end;

destructor TJLSBaseCodec.Destroy;
begin
  DisposeObject(FLossless);
  DisposeObject(FLossy);
  DisposeObject(FMelcode);
  DisposeObject(FJpeg);
  DisposeObject(FBitIO);
  inherited;
end;

function TJLSBaseCodec.Execute: Boolean;
begin
  Result := False;
end;

function TJLSBaseCodec.GetInputStream: TCoreClassStream;
begin
  Result := FInputStream;
end;

function TJLSBaseCodec.GetLimit: Integer;
begin
  Result := FImageInfo.Limit;
end;

function TJLSBaseCodec.GetNEAR: Int;
begin
  Result := FImageInfo._near;
end;

function TJLSBaseCodec.GetOutputStream: TCoreClassStream;
begin
  Result := FOutputStream;
end;

function TJLSBaseCodec.GetRESET: Int;
begin
  Result := FImageInfo.Reset;
end;

{ Initialize A[], B[], C[], and N[] arrays }
procedure TJLSBaseCodec.init_stats(absize: Int);
var
  i, initabstat, slack: Int;
begin
  slack := 1 shl INITABSLACK;
  initabstat := (absize + slack div 2) div slack;
  if (initabstat < MIN_INITABSTAT) then
      initabstat := MIN_INITABSTAT;

  { do the statistics initialization }
  for i := 0 to pred(TOT_CONTEXTS) do
    begin
      FImageInfo.c[i] := 0;
      FImageInfo.b[i] := 0;
      FImageInfo.n[i] := INITNSTAT;
      FImageInfo.a[i] := initabstat;
    end;
end;

function TJLSBaseCodec.GetAlpha: Integer;
begin
  Result := FImageInfo.alpha;
end;

function TJLSBaseCodec.GetComponents: Int;
begin
  Result := FImageInfo.Components;
end;

function TJLSBaseCodec.GetHeight: Int;
begin
  Result := FImageInfo.height;
end;

function TJLSBaseCodec.GetWidth: Int;
begin
  Result := FImageInfo.width;
end;

procedure TJLSBaseCodec.SetAlpha(const Value: Integer);
begin
  FImageInfo.alpha := Value;
end;

procedure TJLSBaseCodec.SetComponents(AValue: Int);
begin
  FImageInfo.Components := AValue;
end;

procedure TJLSBaseCodec.SetHeight(AValue: Int);
begin
  FImageInfo.height := AValue;
end;

procedure TJLSBaseCodec.SetWidth(AValue: Int);
begin
  FImageInfo.width := AValue;
end;

procedure TJLSBaseCodec.set_thresholds(alfa, AnNEAR: Int; T1p, T2p,
  T3p: pint);
var
  LAMBDA, ilambda, quant_: Int;
begin
  ilambda := 256 div alfa;
  quant_ := 2 * AnNEAR + 1;
  FT1 := T1p^;
  FT2 := T2p^;
  FT3 := T3p^;

  if (alfa < 4096) then
      LAMBDA := (alfa + 127) div 256
  else
      LAMBDA := (4096 + 127) div 256;

  if (FT1 <= 0) then
    begin
      { compute lossless default }
      if (IsTrue(LAMBDA)) then
          FT1 := LAMBDA * (BASIC_T1 - 2) + 2
      else begin { alphabet < 8 bits }
          FT1 := BASIC_T1 div ilambda;
          if (FT1 < 2) then
              FT1 := 2;
        end;
      { adjust for lossy }
      FT1 := FT1 + 3 * AnNEAR;

      { check that the default threshold is in bounds }
      if (FT1 < AnNEAR + 1) or (FT1 > (alfa - 1)) then
          FT1 := AnNEAR + 1; { eliminates the threshold }
    end;

  if (FT2 <= 0) then
    begin
      { compute lossless default }
      if (IsTrue(LAMBDA)) then
          FT2 := LAMBDA * (BASIC_T2 - 3) + 3
      else begin
          FT2 := BASIC_T2 div ilambda;
          if (FT2 < 3) then
              FT2 := 3;
        end;
      { adjust for lossy }
      FT2 := FT2 + 5 * AnNEAR;

      { check that the default threshold is in bounds }
      if (FT2 < FT1) or (FT2 > (alfa - 1)) then
          FT2 := FT1; { eliminates the threshold }
    end;

  if (FT3 <= 0) then
    begin
      { compute lossless default }
      if (IsTrue(LAMBDA)) then
          FT3 := LAMBDA * (BASIC_T3 - 4) + 4
      else begin
          FT3 := BASIC_T3 div ilambda;
          if (FT3 < 4) then
              FT3 := 4;
        end;
      { adjust for lossy }
      FT3 := FT3 + 7 * AnNEAR;

      { check that the default threshold is in bounds }
      if (FT3 < FT2) or (FT3 > (alfa - 1)) then
          FT3 := FT2; { eliminates the threshold }
    end;

  T1p^ := FT1;
  T2p^ := FT2;
  T3p^ := FT3;
end;

{ Setup Look Up Tables for quantized gradient merging }
function TJLSBaseCodec.prepareLUTs: Int;
var
  i, j, idx, lmax: Int;
  k: Byte;
  q1, q2, Q3, n1, n2, n3, ineg, sgn: Int;

begin
  Result := 0;

  lmax := Min(FImageInfo.alpha, lutmax);

  { implementation limitation: }
  if (FT3 > lmax - 1) then begin
      DoStatus('ERROR : Sorry, current implementation does not support threshold T3 > %d, got %d', [lmax - 1, FT3]);
      Result := 10;
      Exit;
    end;

  { Build classification tables (lossless or lossy) }

  if (lossy = False) then
    begin

      for i := -lmax + 1 to pred(lmax) do
        begin

          if (i <= -FT3) then { ...... -T3 }
              idx := 7
          else if (i <= -FT2) then { -(T3-1) ... -T2 }
              idx := 5
          else if (i <= -FT1) then { -(T2-1) ... -T1 }
              idx := 3

          else if (i <= -1) then { -(T1-1) ...  -1 }
              idx := 1
          else if (i = 0) then { just 0 }
              idx := 0

          else if (i < FT1) then { 1 ... T1-1 }
              idx := 2
          else if (i < FT2) then { T1 ... T2-1 }
              idx := 4
          else if (i < FT3) then { T2 ... T3-1 }
              idx := 6
          else { T3 ... }
              idx := 8;

          FImageInfo.vLUT[0][i + lutmax] := CREGIONS * CREGIONS * idx;
          FImageInfo.vLUT[1][i + lutmax] := CREGIONS * idx;
          FImageInfo.vLUT[2][i + lutmax] := idx;
        end;

    end
  else begin

      for i := -lmax + 1 to pred(lmax) do
        begin

          if (FImageInfo._near >= (FImageInfo.alpha - 1)) then
              idx := 0 { degenerate case, regardless of thresholds }
          else

            if (i <= -FT3) then { ...... -T3 }
              idx := 7
          else if (i <= -FT2) then { -(T3-1) ... -T2 }
              idx := 5
          else if (i <= -FT1) then { -(T2-1) ... -T1 }
              idx := 3

          else if (i <= -FImageInfo._near - 1) then { -(T1-1) ...  -_near-1 }
              idx := 1
          else if (i <= FImageInfo._near) then { within _near of 0 }
              idx := 0

          else if (i < FT1) then { 1 ... T1-1 }
              idx := 2
          else if (i < FT2) then { T1 ... T2-1 }
              idx := 4
          else if (i < FT3) then { T2 ... T3-1 }
              idx := 6
          else { T3 ... }
              idx := 8;

          FImageInfo.vLUT[0][i + lutmax] := CREGIONS * CREGIONS * idx;
          FImageInfo.vLUT[1][i + lutmax] := CREGIONS * idx;
          FImageInfo.vLUT[2][i + lutmax] := idx;
        end;

    end;

  { prepare context mapping table (symmetric context merging) }
  FImageInfo.classmap[0] := 0;
  j := 0;

  for i := 1 to pred(CONTEXTS1) do
    begin
      n1 := 0;
      n2 := 0;
      n3 := 0;

      if (IsTrue(FImageInfo.classmap[i])) then
          Continue;

      q1 := i div (CREGIONS * CREGIONS);   { first digit }
      q2 := (i div CREGIONS) mod CREGIONS; { second digit }
      Q3 := i mod CREGIONS;                { third digit }

      if (IsTrue(q1 mod 2)) or ((q1 = 0) and IsTrue(q2 mod 2)) or ((q1 = 0) and (q2 = 0) and IsTrue(Q3 mod 2)) then
          sgn := -1
      else
          sgn := 1;

      { compute negative context }
      if IsTrue(q1) then
        begin
          if IsTrue(q1 mod 2) then
              n1 := q1 + 1
          else
              n1 := q1 - 1;
        end;

      if IsTrue(q2) then
        begin
          if IsTrue(q2 mod 2) then
              n2 := q2 + 1
          else
              n2 := q2 - 1;
        end;

      if IsTrue(Q3) then
        begin
          if IsTrue(Q3 mod 2) then
              n3 := Q3 + 1
          else
              n3 := Q3 - 1;
        end;

      ineg := (n1 * CREGIONS + n2) * CREGIONS + n3;
      inc(j); { next class number }
      FImageInfo.classmap[i] := sgn * j;
      FImageInfo.classmap[ineg] := -sgn * j;
    end;

end;

{ prepare quantization tables for _near-lossless quantization }
function TJLSBaseCodec.prepare_qtables(absize: Int; AnNEAR: Int): Int;
var
  Diff, qdiff: Int;
  beta_, quant_: Int;
  arrpos: Int;
begin
  Result := 0;

  quant_ := 2 * AnNEAR + 1;
  beta_ := absize;

  GetMem(FImageInfo.qdiv0, (2 * absize - 1) * SizeOf(Int));
  if (FImageInfo.qdiv0 = nil) then
    begin
      DoStatus('ERROR : qdiv  table');
      Result := 10;
      Exit;
    end;

  FImageInfo.qdiv := Pointer(FImageInfo.qdiv0);
  inc(FImageInfo.qdiv, absize - 1);

  GetMem(FImageInfo.qmul0, (2 * beta_ - 1) * SizeOf(Int));

  if (FImageInfo.qmul0 = nil) then
    begin
      DoStatus('ERROR : qmul  table');
      Result := 10;
      Exit;
    end;

  FImageInfo.qmul := Pointer(FImageInfo.qmul0);
  inc(FImageInfo.qmul, beta_ - 1);

  arrpos := beta_ - 1;

  for Diff := -(absize - 1) to pred(absize) do
    begin
      if (Diff < 0) then
          qdiff := -((AnNEAR - Diff) div quant_)
      else
          qdiff := (AnNEAR + Diff) div quant_;

      FImageInfo.qdiv0^[Diff + arrpos] := qdiff;
    end;

  for qdiff := -(beta_ - 1) to pred(beta_) do
    begin
      Diff := quant * qdiff;
      FImageInfo.qmul0^[qdiff + arrpos] := Diff;
    end;

end;

procedure TJLSBaseCodec.SetInputStream(const Value: TCoreClassStream);
begin
  FInputStream := Value;
  FBitIO.FInputStream := FInputStream;
end;

procedure TJLSBaseCodec.SetInterleaveMode(const Value: Integer);
begin
  color_mode := Value;
end;

procedure TJLSBaseCodec.SetLimit(const Value: Integer);
begin
  FImageInfo.Limit := Value;
end;

procedure TJLSBaseCodec.SetMAXVAL(const Value: Int);
begin
  if Value = 0 then
      alpha0 := DEF_ALPHA
  else
      alpha0 := Value;
end;

procedure TJLSBaseCodec.SetNEAR(const Value: Int);
begin
  FImageInfo._near := Value;
end;

procedure TJLSBaseCodec.SetOutputStream(const Value: TCoreClassStream);
begin
  FOutputStream := Value;
  FBitIO.FOutputStream := FOutputStream;
end;

procedure TJLSBaseCodec.SetRESET(const Value: Int);
begin
  { Reset value }
  if (FImageInfo.Reset <> DEFAULT_RESET) and (Value > 0) then
    begin
      FImageInfo.Reset := Value;
      need_lse := 1;
    end;
end;

procedure TJLSBaseCodec.SetRestartInterval(const Value: Integer);
begin
  { Enable use of Restart Markers }
  need_restart := 1;
end;

procedure TJLSBaseCodec.SetT1(const Value: Int);
begin
  if Value <> 0 then
    begin
      FT1 := Value;
      need_lse := 1;
    end;
end;

procedure TJLSBaseCodec.SetT2(const Value: Int);
begin
  if Value <> 0 then
    begin
      FT2 := Value;
      need_lse := 1;
    end;
end;

procedure TJLSBaseCodec.SetT3(const Value: Int);
begin
  if Value <> 0 then
    begin
      FT3 := Value;
      need_lse := 1;
    end;
end;

end. 
 
 
