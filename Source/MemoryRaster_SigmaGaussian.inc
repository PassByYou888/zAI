type
  TSigmaGaussianBuffer = array [0 .. MaxInt div SizeOf(TGeoFloat) - 1] of TGeoFloat;
  PSigmaGaussianBuffer = ^TSigmaGaussianBuffer;

  TSigmaGaussianKernel = record
    SigmaWidth, SigmaCenter: Integer;
    Weights: PSigmaGaussianBuffer;
  end;

procedure BuildSigmaGaussianKernel(const SIGMA: TGeoFloat; const SigmaGaussianKernelFactor: Integer; var kernel: TSigmaGaussianKernel);
var
  exp_coeff, wsum, fac: TGeoFloat;
  i: Integer;
  p: PSigmaGaussianBuffer;
begin
  kernel.SigmaWidth := Ceil(0.3 * (SIGMA * 0.5 - 1) + 0.8) * SigmaGaussianKernelFactor;
  if (kernel.SigmaWidth mod 2 = 0) then
      inc(kernel.SigmaWidth);

  kernel.Weights := System.GetMemory(SizeOf(TGeoFloat) * kernel.SigmaWidth);

  kernel.SigmaCenter := kernel.SigmaWidth div 2;
  p := @kernel.Weights^[kernel.SigmaCenter];
  p^[0] := 1;

  exp_coeff := -1.0 / (SIGMA * SIGMA * 2);
  wsum := 1;

  for i := 1 to kernel.SigmaCenter do
    begin
      p^[i] := Exp(i * i * exp_coeff);
      wsum := wsum + p^[i] * 2;
    end;

  fac := 1.0 / wsum;
  p^[0] := fac;

  for i := 1 to kernel.SigmaCenter do
    begin
      kernel.Weights^[i + kernel.SigmaCenter] := p^[i] * fac;
      kernel.Weights^[-i + kernel.SigmaCenter] := p^[i];
    end;
end;

procedure PixelSigmaGaussianRow(const sour, dest: PRColorArray; const L: Integer; var k: TSigmaGaussianKernel);
  function GK(const V, L, H: Integer): Integer; inline;
  begin
    Result := V;
    if Result > H then
        Result := H
    else if Result < L then
        Result := L;
  end;

var
  j, n: Integer;
  V: TVector4;
begin
  for j := 0 to L - 1 do
    begin
      V := 0;
      for n := -k.SigmaCenter to k.SigmaCenter do
          V := V + (RColor2Vector4(sour^[GK(j + n, 0, L - 1)]) * k.Weights^[n + k.SigmaCenter]);
      dest^[j] := RColor(V.buff);
    end;
end;

procedure PixelSigmaGaussianSampler(Parallel_: Boolean; const sour, dest: TMemoryRaster; const SIGMA: TGeoFloat; const SigmaGaussianKernelFactor: Integer); overload;
var
  W, H: Integer;
  k: TSigmaGaussianKernel;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelForH(pass: Integer);
  begin
    PixelSigmaGaussianRow(sour.ScanLine[pass], dest.ScanLine[pass], W, k);
  end;
  procedure Nested_ParallelForW(pass: Integer);
  var
    j: Integer;
    p: PRColor;
    LPixels: PRColorArray;
  begin
    LPixels := System.GetMemory(SizeOf(TRColor) * H);

    p := dest.PixelPtr[pass, 0];
    for j := 0 to H - 1 do
      begin
        LPixels^[j] := p^;
        inc(p, W);
      end;
    PixelSigmaGaussianRow(LPixels, LPixels, H, k);
    p := dest.PixelPtr[pass, 0];
    for j := 0 to H - 1 do
      begin
        p^ := LPixels^[j];
        inc(p, W);
      end;

    System.FreeMemory(LPixels);
  end;
{$ENDIF FPC}

{$ENDIF Parallel}
  procedure DoFor;
  var
    pass: Integer;
    j: Integer;
    p: PRColor;
    LPixels: PRColorArray;
  begin
    for pass := 0 to H - 1 do
        PixelSigmaGaussianRow(sour.ScanLine[pass], dest.ScanLine[pass], W, k);

    LPixels := System.GetMemory(SizeOf(TRColor) * H);
    for pass := 0 to W - 1 do
      begin
        p := dest.PixelPtr[pass, 0];
        for j := 0 to H - 1 do
          begin
            LPixels^[j] := p^;
            inc(p, W);
          end;
        PixelSigmaGaussianRow(LPixels, LPixels, H, k);
        p := dest.PixelPtr[pass, 0];
        for j := 0 to H - 1 do
          begin
            p^ := LPixels^[j];
            inc(p, W);
          end;
      end;

    System.FreeMemory(LPixels);
  end;

begin
  W := sour.Width;
  H := sour.Height;

  if sour <> dest then
      dest.SetSize(W, H);

  BuildSigmaGaussianKernel(SIGMA, SigmaGaussianKernelFactor, k);

  if Parallel_ then
    begin
{$IFDEF Parallel}
{$IFDEF FPC}
      FPCParallelFor(@Nested_ParallelForH, 0, H - 1);
      FPCParallelFor(@Nested_ParallelForW, 0, W - 1);
{$ELSE FPC}
      DelphiParallelFor(0, H - 1, procedure(pass: Integer)
        begin
          PixelSigmaGaussianRow(sour.ScanLine[pass], dest.ScanLine[pass], W, k);
        end);
      DelphiParallelFor(0, W - 1, procedure(pass: Integer)
        var
          j: Integer;
          p: PRColor;
          LPixels: PRColorArray;
        begin
          LPixels := System.GetMemory(SizeOf(TRColor) * H);

          p := dest.PixelPtr[pass, 0];
          for j := 0 to H - 1 do
            begin
              LPixels^[j] := p^;
              inc(p, W);
            end;
          PixelSigmaGaussianRow(LPixels, LPixels, H, k);
          p := dest.PixelPtr[pass, 0];
          for j := 0 to H - 1 do
            begin
              p^ := LPixels^[j];
              inc(p, W);
            end;

          System.FreeMemory(LPixels);
        end);
{$ENDIF FPC}
{$ELSE Parallel}
      DoFor;
{$ENDIF Parallel}
    end
  else
    begin
      DoFor;
    end;
  System.FreeMemory(k.Weights);
end;

procedure PixelSigmaGaussianSampler(const sour, dest: TMemoryRaster; const SIGMA: TGeoFloat; const SigmaGaussianKernelFactor: Integer); overload;
begin
  PixelSigmaGaussianSampler(True, sour, dest, SIGMA, SigmaGaussianKernelFactor);
end;

procedure MorphSigmaGaussianRow(const sour, dest: PMorphomaticsBits; const L: Integer; var k: TSigmaGaussianKernel);
  function GK(const V, L, H: Integer): Integer; inline;
  begin
    Result := V;
    if Result > H then
        Result := H
    else if Result < L then
        Result := L;
  end;

var
  j, n: Integer;
  V: TMorphomaticsValue;
begin
  for j := 0 to L - 1 do
    begin
      V := 0;
      for n := -k.SigmaCenter to k.SigmaCenter do
          V := V + (sour^[GK(j + n, 0, L - 1)]) * k.Weights^[n + k.SigmaCenter];
      dest^[j] := V;
    end;
end;

procedure MorphSigmaGaussianSampler(Parallel_: Boolean; const sour, dest: TMorphomatics; const SIGMA: TGeoFloat; const SigmaGaussianKernelFactor: Integer); overload;
var
  W, H: Integer;
  k: TSigmaGaussianKernel;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelForH(pass: Integer);
  begin
    MorphSigmaGaussianRow(sour.ScanLine[pass], dest.ScanLine[pass], W, k);
  end;
  procedure Nested_ParallelForW(pass: Integer);
  var
    j: Integer;
    p: PMorphomaticsValue;
    LPixels: PMorphomaticsBits;
  begin
    LPixels := System.GetMemory(SizeOf(TRColor) * H);

    p := dest.PixelPtr[pass, 0];
    for j := 0 to H - 1 do
      begin
        LPixels^[j] := p^;
        inc(p, W);
      end;
    MorphSigmaGaussianRow(LPixels, LPixels, H, k);
    p := dest.PixelPtr[pass, 0];
    for j := 0 to H - 1 do
      begin
        p^ := LPixels^[j];
        inc(p, W);
      end;

    System.FreeMemory(LPixels);
  end;
{$ENDIF FPC}
{$ENDIF Parallel}
  procedure DoFor;
  var
    pass: Integer;
    j: Integer;
    p: PMorphomaticsValue;
    LPixels: PMorphomaticsBits;
  begin
    for pass := 0 to H - 1 do
        MorphSigmaGaussianRow(sour.ScanLine[pass], dest.ScanLine[pass], W, k);

    LPixels := System.GetMemory(SizeOf(TRColor) * H);
    for pass := 0 to W - 1 do
      begin
        p := dest.PixelPtr[pass, 0];
        for j := 0 to H - 1 do
          begin
            LPixels^[j] := p^;
            inc(p, W);
          end;
        MorphSigmaGaussianRow(LPixels, LPixels, H, k);
        p := dest.PixelPtr[pass, 0];
        for j := 0 to H - 1 do
          begin
            p^ := LPixels^[j];
            inc(p, W);
          end;
      end;

    System.FreeMemory(LPixels);
  end;

begin
  W := sour.Width;
  H := sour.Height;

  if sour <> dest then
      dest.SetSize(W, H);

  BuildSigmaGaussianKernel(SIGMA, SigmaGaussianKernelFactor, k);

  if Parallel_ then
    begin
{$IFDEF Parallel}
{$IFDEF FPC}
      FPCParallelFor(@Nested_ParallelForH, 0, H - 1);
      FPCParallelFor(@Nested_ParallelForW, 0, W - 1);
{$ELSE FPC}
      DelphiParallelFor(0, H - 1, procedure(pass: Integer)
        begin
          MorphSigmaGaussianRow(sour.ScanLine[pass], dest.ScanLine[pass], W, k);
        end);
      DelphiParallelFor(0, W - 1, procedure(pass: Integer)
        var
          j: Integer;
          p: PMorphomaticsValue;
          LPixels: PMorphomaticsBits;
        begin
          LPixels := System.GetMemory(SizeOf(TRColor) * H);

          p := dest.PixelPtr[pass, 0];
          for j := 0 to H - 1 do
            begin
              LPixels^[j] := p^;
              inc(p, W);
            end;
          MorphSigmaGaussianRow(LPixels, LPixels, H, k);
          p := dest.PixelPtr[pass, 0];
          for j := 0 to H - 1 do
            begin
              p^ := LPixels^[j];
              inc(p, W);
            end;

          System.FreeMemory(LPixels);
        end);
{$ENDIF FPC}
{$ELSE Parallel}
      DoFor;
{$ENDIF Parallel}
    end
  else
    begin
      DoFor;
    end;
  System.FreeMemory(k.Weights);
end;

procedure MorphSigmaGaussianSampler(const sour, dest: TMorphomatics; const SIGMA: TGeoFloat; const SigmaGaussianKernelFactor: Integer); overload;
begin
  MorphSigmaGaussianSampler(True, sour, dest, SIGMA, SigmaGaussianKernelFactor);
end;

procedure TestPixelSigmaGaussian(inputFile, outputFile: SystemString);
var
  M: TMemoryRaster;
begin
  M := NewRasterFromFile(inputFile);
  PixelSigmaGaussianSampler(M, M, 10.0, 3);
  SaveRaster(M, outputFile);
  disposeObject(M);
end;
