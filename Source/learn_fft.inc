const
  FTBasePlanEntrySize = 8;
  FTBaseCFFTTask = 0;
  FTBaseRFHTTask = 1;
  FTBaseRFFTTask = 2;
  FFTCooleyTukeyPlan = 0;
  FFTBluesteinPlan = 1;
  FFTCodeletPlan = 2;
  FHTCooleyTukeyPlan = 3;
  FHTCodeletPlan = 4;
  FFTRealCooleyTukeyPlan = 5;
  FFTEmptyPlan = 6;
  FHTN2Plan = 999;
  FTBaseUpdateTw = 4;
  FTBaseCodeletMax = 5;
  FTBaseCodeletRecommended = 5;
  FTBaseInefficiencyFactor = 1.3;
  FTBaseMaxSmoothFactor = 5;

procedure FTBaseGeneratePlanRec(n: TLInt; TaskType: TLInt;
  var Plan: TFTPlan; var PlanSize: TLInt;
  var PrecomputedSize: TLInt; var PlanArraySize: TLInt;
  var TmpMemSize: TLInt; var StackMemSize: TLInt; StackPtr: TLInt); forward;

procedure FTBasePrecomputePlanRec(var Plan: TFTPlan; EntryOffset: TLInt; StackPtr: TLInt); forward;
procedure FFTTwCalc(var a: TLVec; AOffset: TLInt; n1: TLInt; n2: TLInt); forward;
procedure InternalComplexLinTranspose(var a: TLVec; M: TLInt; n: TLInt; AStart: TLInt; var Buf: TLVec); forward;
procedure InternalRealLinTranspose(var a: TLVec; M: TLInt; n: TLInt; AStart: TLInt; var Buf: TLVec); forward;
procedure FFTICLTRec(var a: TLVec; AStart: TLInt; AStride: TLInt; var b: TLVec; BStart: TLInt; BStride: TLInt; M: TLInt; n: TLInt); forward;
procedure FFTIRLTRec(var a: TLVec; AStart: TLInt; AStride: TLInt; var b: TLVec; BStart: TLInt; BStride: TLInt; M: TLInt; n: TLInt); forward;
procedure FTBaseFindSmoothRec(n: TLInt; Seed: TLInt; LeastFactor: TLInt; var Best: TLInt); forward;
procedure FFTArrayResize(var a: TLIVec; var ASize: TLInt; NewASize: TLInt); forward;
procedure RefFHT(var a: TLVec; n: TLInt; Offs: TLInt); forward;

(* ************************************************************************
  This subroutine generates FFT plan - a decomposition of a N-length FFT to
  the more simpler operations. Plan consists of the root entry and the child
  entries.

  Subroutine parameters:
  N               task size

  Output parameters:
  Plan            plan
  ************************************************************************ *)
procedure FTBaseGenerateComplexFFTPlan(n: TLInt; var Plan: TFTPlan);
var
  PlanArraySize: TLInt;
  PlanSize: TLInt;
  PrecomputedSize: TLInt;
  TmpMemSize: TLInt;
  StackMemSize: TLInt;
  StackPtr: TLInt;
begin
  PlanArraySize := 1;
  PlanSize := 0;
  PrecomputedSize := 0;
  StackMemSize := 0;
  StackPtr := 0;
  TmpMemSize := 2 * n;
  SetLength(Plan.Plan, PlanArraySize);
  FTBaseGeneratePlanRec(n, FTBaseCFFTTask, Plan, PlanSize, PrecomputedSize, PlanArraySize, TmpMemSize, StackMemSize, StackPtr);
  Assert(StackPtr = 0, 'Internal error in FTBaseGenerateComplexFFTPlan: stack ptr!');
  SetLength(Plan.StackBuf, Max(StackMemSize, 1));
  SetLength(Plan.TmpBuf, Max(TmpMemSize, 1));
  SetLength(Plan.Precomputed, Max(PrecomputedSize, 1));
  StackPtr := 0;
  FTBasePrecomputePlanRec(Plan, 0, StackPtr);
  Assert(StackPtr = 0, 'Internal error in FTBaseGenerateComplexFFTPlan: stack ptr!');
end;

(* ************************************************************************
  Generates real FFT plan
  ************************************************************************ *)
procedure FTBaseGenerateRealFFTPlan(n: TLInt; var Plan: TFTPlan);
var
  PlanArraySize: TLInt;
  PlanSize: TLInt;
  PrecomputedSize: TLInt;
  TmpMemSize: TLInt;
  StackMemSize: TLInt;
  StackPtr: TLInt;
begin
  PlanArraySize := 1;
  PlanSize := 0;
  PrecomputedSize := 0;
  StackMemSize := 0;
  StackPtr := 0;
  TmpMemSize := 2 * n;
  SetLength(Plan.Plan, PlanArraySize);
  FTBaseGeneratePlanRec(n, FTBaseRFFTTask, Plan, PlanSize, PrecomputedSize, PlanArraySize, TmpMemSize, StackMemSize, StackPtr);
  Assert(StackPtr = 0, 'Internal error in FTBaseGenerateRealFFTPlan: stack ptr!');
  SetLength(Plan.StackBuf, Max(StackMemSize, 1));
  SetLength(Plan.TmpBuf, Max(TmpMemSize, 1));
  SetLength(Plan.Precomputed, Max(PrecomputedSize, 1));
  StackPtr := 0;
  FTBasePrecomputePlanRec(Plan, 0, StackPtr);
  Assert(StackPtr = 0, 'Internal error in FTBaseGenerateRealFFTPlan: stack ptr!');
end;

(* ************************************************************************
  Generates real FHT plan
  ************************************************************************ *)
procedure FTBaseGenerateRealFHTPlan(n: TLInt; var Plan: TFTPlan);
var
  PlanArraySize: TLInt;
  PlanSize: TLInt;
  PrecomputedSize: TLInt;
  TmpMemSize: TLInt;
  StackMemSize: TLInt;
  StackPtr: TLInt;
begin
  PlanArraySize := 1;
  PlanSize := 0;
  PrecomputedSize := 0;
  StackMemSize := 0;
  StackPtr := 0;
  TmpMemSize := n;
  SetLength(Plan.Plan, PlanArraySize);
  FTBaseGeneratePlanRec(n, FTBaseRFHTTask, Plan, PlanSize, PrecomputedSize, PlanArraySize, TmpMemSize, StackMemSize, StackPtr);
  Assert(StackPtr = 0, 'Internal error in FTBaseGenerateRealFHTPlan: stack ptr!');
  SetLength(Plan.StackBuf, Max(StackMemSize, 1));
  SetLength(Plan.TmpBuf, Max(TmpMemSize, 1));
  SetLength(Plan.Precomputed, Max(PrecomputedSize, 1));
  StackPtr := 0;
  FTBasePrecomputePlanRec(Plan, 0, StackPtr);
  Assert(StackPtr = 0, 'Internal error in FTBaseGenerateRealFHTPlan: stack ptr!');
end;

(* ************************************************************************
  This subroutine executes FFT/FHT plan.

  If Plan is a:
  * TLComplex FFT plan  -   sizeof(A)=2*N,
  A contains interleaved real/imaginary values
  * real FFT plan     -   sizeof(A)=2*N,
  A contains real values interleaved with zeros
  * real FHT plan     -   sizeof(A)=2*N,
  A contains real values interleaved with zeros
  ************************************************************************ *)
procedure FTBaseExecutePlan(var a: TLVec; AOffset: TLInt;
  n: TLInt; var Plan: TFTPlan);
var
  StackPtr: TLInt;
begin
  StackPtr := 0;
  FTBaseExecutePlanRec(a, AOffset, Plan, 0, StackPtr);
end;

(* ************************************************************************
  Recurrent subroutine for the FTBaseExecutePlan

  Parameters:
  A           FFT'ed array
  AOffset     offset of the FFT'ed part (distance is measured in doubles)
  ************************************************************************ *)
procedure FTBaseExecutePlanRec(var a: TLVec; AOffset: TLInt;
  var Plan: TFTPlan; EntryOffset: TLInt; StackPtr: TLInt);
var
  i: TLInt;
  j: TLInt;
  k: TLInt;
  n1: TLInt;
  n2: TLInt;
  n: TLInt;
  M: TLInt;
  Offs: TLInt;
  Offs1: TLInt;
  Offs2: TLInt;
  OffsA: TLInt;
  OffsB: TLInt;
  OffsP: TLInt;
  HK: TLFloat;
  HNK: TLFloat;
  x: TLFloat;
  y: TLFloat;
  Bx: TLFloat;
  By: TLFloat;
  EmptyArray: TLVec;
  A0X: TLFloat;
  A0Y: TLFloat;
  A1X: TLFloat;
  A1Y: TLFloat;
  A2X: TLFloat;
  A2Y: TLFloat;
  A3X: TLFloat;
  A3Y: TLFloat;
  v0: TLFloat;
  v1: TLFloat;
  v2: TLFloat;
  v3: TLFloat;
  T1X: TLFloat;
  T1Y: TLFloat;
  T2X: TLFloat;
  T2Y: TLFloat;
  T3X: TLFloat;
  T3Y: TLFloat;
  T4X: TLFloat;
  T4Y: TLFloat;
  T5X: TLFloat;
  T5Y: TLFloat;
  M1X: TLFloat;
  M1Y: TLFloat;
  M2X: TLFloat;
  M2Y: TLFloat;
  M3X: TLFloat;
  M3Y: TLFloat;
  M4X: TLFloat;
  M4Y: TLFloat;
  M5X: TLFloat;
  M5Y: TLFloat;
  S1X: TLFloat;
  S1Y: TLFloat;
  S2X: TLFloat;
  S2Y: TLFloat;
  S3X: TLFloat;
  S3Y: TLFloat;
  S4X: TLFloat;
  S4Y: TLFloat;
  S5X: TLFloat;
  S5Y: TLFloat;
  c1: TLFloat;
  c2: TLFloat;
  c3: TLFloat;
  c4: TLFloat;
  C5: TLFloat;
  tmp: TLVec;
begin
  if Plan.Plan[EntryOffset + 3] = FFTEmptyPlan then
    begin
      Exit;
    end;
  if Plan.Plan[EntryOffset + 3] = FFTCooleyTukeyPlan then
    begin

      //
      // Cooley-Tukey plan
      // * transposition
      // * row-wise FFT
      // * twiddle factors:
      // - TwBase is a basis twiddle factor for I=1, J=1
      // - TwRow is a twiddle factor for a second element in a row (J=1)
      // - Tw is a twiddle factor for a current element
      // * transposition again
      // * row-wise FFT again
      //
      n1 := Plan.Plan[EntryOffset + 1];
      n2 := Plan.Plan[EntryOffset + 2];
      InternalComplexLinTranspose(a, n1, n2, AOffset, Plan.TmpBuf);
      i := 0;
      while i <= n2 - 1 do
        begin
          FTBaseExecutePlanRec(a, AOffset + i * n1 * 2, Plan,
            Plan.Plan[EntryOffset + 5], StackPtr);
          inc(i);
        end;
      FFTTwCalc(a, AOffset, n1, n2);
      InternalComplexLinTranspose(a, n2, n1, AOffset, Plan.TmpBuf);
      i := 0;
      while i <= n1 - 1 do
        begin
          FTBaseExecutePlanRec(a, AOffset + i * n2 * 2, Plan,
            Plan.Plan[EntryOffset + 6], StackPtr);
          inc(i);
        end;
      InternalComplexLinTranspose(a, n1, n2, AOffset, Plan.TmpBuf);
      Exit;
    end;
  if Plan.Plan[EntryOffset + 3] = FFTRealCooleyTukeyPlan then
    begin

      //
      // Cooley-Tukey plan
      // * transposition
      // * row-wise FFT
      // * twiddle factors:
      // - TwBase is a basis twiddle factor for I=1, J=1
      // - TwRow is a twiddle factor for a second element in a row (J=1)
      // - Tw is a twiddle factor for a current element
      // * transposition again
      // * row-wise FFT again
      //
      n1 := Plan.Plan[EntryOffset + 1];
      n2 := Plan.Plan[EntryOffset + 2];
      InternalComplexLinTranspose(a, n2, n1, AOffset, Plan.TmpBuf);
      i := 0;
      while i <= n1 div 2 - 1 do
        begin

          //
          // pack two adjacent smaller real FFT's together,
          // make one TLComplex FFT,
          // unpack result
          //
          Offs := AOffset + 2 * i * n2 * 2;
          k := 0;
          while k <= n2 - 1 do
            begin
              a[Offs + 2 * k + 1] := a[Offs + 2 * n2 + 2 * k + 0];
              inc(k);
            end;
          FTBaseExecutePlanRec(a, Offs, Plan, Plan.Plan[EntryOffset + 6], StackPtr);
          Plan.TmpBuf[0] := a[Offs + 0];
          Plan.TmpBuf[1] := 0;
          Plan.TmpBuf[2 * n2 + 0] := a[Offs + 1];
          Plan.TmpBuf[2 * n2 + 1] := 0;
          k := 1;
          while k <= n2 - 1 do
            begin
              Offs1 := 2 * k;
              Offs2 := 2 * n2 + 2 * k;
              HK := a[Offs + 2 * k + 0];
              HNK := a[Offs + 2 * (n2 - k) + 0];
              Plan.TmpBuf[Offs1 + 0] := +0.5 * (HK + HNK);
              Plan.TmpBuf[Offs2 + 1] := -0.5 * (HK - HNK);
              HK := a[Offs + 2 * k + 1];
              HNK := a[Offs + 2 * (n2 - k) + 1];
              Plan.TmpBuf[Offs2 + 0] := +0.5 * (HK + HNK);
              Plan.TmpBuf[Offs1 + 1] := +0.5 * (HK - HNK);
              inc(k);
            end;
          APVMove(@a[0], Offs, Offs + 2 * n2 * 2 - 1, @Plan.TmpBuf[0], 0,
            2 * n2 * 2 - 1);
          inc(i);
        end;
      if n1 mod 2 <> 0 then
        begin
          FTBaseExecutePlanRec(a, AOffset + (n1 - 1) * n2 * 2, Plan,
            Plan.Plan[EntryOffset + 6], StackPtr);
        end;
      FFTTwCalc(a, AOffset, n2, n1);
      InternalComplexLinTranspose(a, n1, n2, AOffset, Plan.TmpBuf);
      i := 0;
      while i <= n2 - 1 do
        begin
          FTBaseExecutePlanRec(a, AOffset + i * n1 * 2, Plan,
            Plan.Plan[EntryOffset + 5], StackPtr);
          inc(i);
        end;
      InternalComplexLinTranspose(a, n2, n1, AOffset, Plan.TmpBuf);
      Exit;
    end;
  if Plan.Plan[EntryOffset + 3] = FHTCooleyTukeyPlan then
    begin

      //
      // Cooley-Tukey FHT plan:
      // * transpose                    \
      // * smaller FHT's                |
      // * pre-process                  |
      // * multiply by twiddle factors  | corresponds to multiplication by H1
      // * post-process                 |
      // * transpose again              /
      // * multiply by H2 (smaller FHT's)
      // * final transposition
      //
      // For more details see Vitezslav Vesely, "Fast algorithms
      // of Fourier and Hartley transform and their implementation in MATLAB",

      // page 31.
      //
      n1 := Plan.Plan[EntryOffset + 1];
      n2 := Plan.Plan[EntryOffset + 2];
      n := n1 * n2;
      InternalRealLinTranspose(a, n1, n2, AOffset, Plan.TmpBuf);
      i := 0;
      while i <= n2 - 1 do
        begin
          FTBaseExecutePlanRec(a, AOffset + i * n1, Plan,
            Plan.Plan[EntryOffset + 5], StackPtr);
          inc(i);
        end;
      i := 0;
      while i <= n2 - 1 do
        begin
          j := 0;
          while j <= n1 - 1 do
            begin
              OffsA := AOffset + i * n1;
              HK := a[OffsA + j];
              HNK := a[OffsA + (n1 - j) mod n1];
              Offs := 2 * (i * n1 + j);
              Plan.TmpBuf[Offs + 0] := -0.5 * (HNK - HK);
              Plan.TmpBuf[Offs + 1] := +0.5 * (HK + HNK);
              inc(j);
            end;
          inc(i);
        end;
      FFTTwCalc(Plan.TmpBuf, 0, n1, n2);
      j := 0;
      while j <= n1 - 1 do
        begin
          a[AOffset + j] := Plan.TmpBuf[2 * j + 0] + Plan.TmpBuf[2 * j + 1];
          inc(j);
        end;
      if n2 mod 2 = 0 then
        begin
          Offs := 2 * (n2 div 2) * n1;
          OffsA := AOffset + n2 div 2 * n1;
          j := 0;
          while j <= n1 - 1 do
            begin
              a[OffsA + j] := Plan.TmpBuf[Offs + 2 * j + 0] + Plan.TmpBuf
                [Offs + 2 * j + 1];
              inc(j);
            end;
        end;
      i := 1;
      while i <= (n2 + 1) div 2 - 1 do
        begin
          Offs := 2 * i * n1;
          Offs2 := 2 * (n2 - i) * n1;
          OffsA := AOffset + i * n1;
          j := 0;
          while j <= n1 - 1 do
            begin
              a[OffsA + j] := Plan.TmpBuf[Offs + 2 * j + 1] + Plan.TmpBuf
                [Offs2 + 2 * j + 0];
              inc(j);
            end;
          OffsA := AOffset + (n2 - i) * n1;
          j := 0;
          while j <= n1 - 1 do
            begin
              a[OffsA + j] := Plan.TmpBuf[Offs + 2 * j + 0] + Plan.TmpBuf
                [Offs2 + 2 * j + 1];
              inc(j);
            end;
          inc(i);
        end;
      InternalRealLinTranspose(a, n2, n1, AOffset, Plan.TmpBuf);
      i := 0;
      while i <= n1 - 1 do
        begin
          FTBaseExecutePlanRec(a, AOffset + i * n2, Plan,
            Plan.Plan[EntryOffset + 6], StackPtr);
          inc(i);
        end;
      InternalRealLinTranspose(a, n1, n2, AOffset, Plan.TmpBuf);
      Exit;
    end;
  if Plan.Plan[EntryOffset + 3] = FHTN2Plan then
    begin

      //
      // Cooley-Tukey FHT plan
      //
      n1 := Plan.Plan[EntryOffset + 1];
      n2 := Plan.Plan[EntryOffset + 2];
      n := n1 * n2;
      RefFHT(a, n, AOffset);
      Exit;
    end;
  if Plan.Plan[EntryOffset + 3] = FFTCodeletPlan then
    begin
      n1 := Plan.Plan[EntryOffset + 1];
      n2 := Plan.Plan[EntryOffset + 2];
      n := n1 * n2;
      if n = 2 then
        begin
          A0X := a[AOffset + 0];
          A0Y := a[AOffset + 1];
          A1X := a[AOffset + 2];
          A1Y := a[AOffset + 3];
          v0 := A0X + A1X;
          v1 := A0Y + A1Y;
          v2 := A0X - A1X;
          v3 := A0Y - A1Y;
          a[AOffset + 0] := v0;
          a[AOffset + 1] := v1;
          a[AOffset + 2] := v2;
          a[AOffset + 3] := v3;
          Exit;
        end;
      if n = 3 then
        begin
          Offs := Plan.Plan[EntryOffset + 7];
          c1 := Plan.Precomputed[Offs + 0];
          c2 := Plan.Precomputed[Offs + 1];
          A0X := a[AOffset + 0];
          A0Y := a[AOffset + 1];
          A1X := a[AOffset + 2];
          A1Y := a[AOffset + 3];
          A2X := a[AOffset + 4];
          A2Y := a[AOffset + 5];
          T1X := A1X + A2X;
          T1Y := A1Y + A2Y;
          A0X := A0X + T1X;
          A0Y := A0Y + T1Y;
          M1X := c1 * T1X;
          M1Y := c1 * T1Y;
          M2X := c2 * (A1Y - A2Y);
          M2Y := c2 * (A2X - A1X);
          S1X := A0X + M1X;
          S1Y := A0Y + M1Y;
          A1X := S1X + M2X;
          A1Y := S1Y + M2Y;
          A2X := S1X - M2X;
          A2Y := S1Y - M2Y;
          a[AOffset + 0] := A0X;
          a[AOffset + 1] := A0Y;
          a[AOffset + 2] := A1X;
          a[AOffset + 3] := A1Y;
          a[AOffset + 4] := A2X;
          a[AOffset + 5] := A2Y;
          Exit;
        end;
      if n = 4 then
        begin
          A0X := a[AOffset + 0];
          A0Y := a[AOffset + 1];
          A1X := a[AOffset + 2];
          A1Y := a[AOffset + 3];
          A2X := a[AOffset + 4];
          A2Y := a[AOffset + 5];
          A3X := a[AOffset + 6];
          A3Y := a[AOffset + 7];
          T1X := A0X + A2X;
          T1Y := A0Y + A2Y;
          T2X := A1X + A3X;
          T2Y := A1Y + A3Y;
          M2X := A0X - A2X;
          M2Y := A0Y - A2Y;
          M3X := A1Y - A3Y;
          M3Y := A3X - A1X;
          a[AOffset + 0] := T1X + T2X;
          a[AOffset + 1] := T1Y + T2Y;
          a[AOffset + 4] := T1X - T2X;
          a[AOffset + 5] := T1Y - T2Y;
          a[AOffset + 2] := M2X + M3X;
          a[AOffset + 3] := M2Y + M3Y;
          a[AOffset + 6] := M2X - M3X;
          a[AOffset + 7] := M2Y - M3Y;
          Exit;
        end;
      if n = 5 then
        begin
          Offs := Plan.Plan[EntryOffset + 7];
          c1 := Plan.Precomputed[Offs + 0];
          c2 := Plan.Precomputed[Offs + 1];
          c3 := Plan.Precomputed[Offs + 2];
          c4 := Plan.Precomputed[Offs + 3];
          C5 := Plan.Precomputed[Offs + 4];
          T1X := a[AOffset + 2] + a[AOffset + 8];
          T1Y := a[AOffset + 3] + a[AOffset + 9];
          T2X := a[AOffset + 4] + a[AOffset + 6];
          T2Y := a[AOffset + 5] + a[AOffset + 7];
          T3X := a[AOffset + 2] - a[AOffset + 8];
          T3Y := a[AOffset + 3] - a[AOffset + 9];
          T4X := a[AOffset + 6] - a[AOffset + 4];
          T4Y := a[AOffset + 7] - a[AOffset + 5];
          T5X := T1X + T2X;
          T5Y := T1Y + T2Y;
          a[AOffset + 0] := a[AOffset + 0] + T5X;
          a[AOffset + 1] := a[AOffset + 1] + T5Y;
          M1X := c1 * T5X;
          M1Y := c1 * T5Y;
          M2X := c2 * (T1X - T2X);
          M2Y := c2 * (T1Y - T2Y);
          M3X := -c3 * (T3Y + T4Y);
          M3Y := c3 * (T3X + T4X);
          M4X := -c4 * T4Y;
          M4Y := c4 * T4X;
          M5X := -C5 * T3Y;
          M5Y := C5 * T3X;
          S3X := M3X - M4X;
          S3Y := M3Y - M4Y;
          S5X := M3X + M5X;
          S5Y := M3Y + M5Y;
          S1X := a[AOffset + 0] + M1X;
          S1Y := a[AOffset + 1] + M1Y;
          S2X := S1X + M2X;
          S2Y := S1Y + M2Y;
          S4X := S1X - M2X;
          S4Y := S1Y - M2Y;
          a[AOffset + 2] := S2X + S3X;
          a[AOffset + 3] := S2Y + S3Y;
          a[AOffset + 4] := S4X + S5X;
          a[AOffset + 5] := S4Y + S5Y;
          a[AOffset + 6] := S4X - S5X;
          a[AOffset + 7] := S4Y - S5Y;
          a[AOffset + 8] := S2X - S3X;
          a[AOffset + 9] := S2Y - S3Y;
          Exit;
        end;
    end;
  if Plan.Plan[EntryOffset + 3] = FHTCodeletPlan then
    begin
      n1 := Plan.Plan[EntryOffset + 1];
      n2 := Plan.Plan[EntryOffset + 2];
      n := n1 * n2;
      if n = 2 then
        begin
          A0X := a[AOffset + 0];
          A1X := a[AOffset + 1];
          a[AOffset + 0] := A0X + A1X;
          a[AOffset + 1] := A0X - A1X;
          Exit;
        end;
      if n = 3 then
        begin
          Offs := Plan.Plan[EntryOffset + 7];
          c1 := Plan.Precomputed[Offs + 0];
          c2 := Plan.Precomputed[Offs + 1];
          A0X := a[AOffset + 0];
          A1X := a[AOffset + 1];
          A2X := a[AOffset + 2];
          T1X := A1X + A2X;
          A0X := A0X + T1X;
          M1X := c1 * T1X;
          M2Y := c2 * (A2X - A1X);
          S1X := A0X + M1X;
          a[AOffset + 0] := A0X;
          a[AOffset + 1] := S1X - M2Y;
          a[AOffset + 2] := S1X + M2Y;
          Exit;
        end;
      if n = 4 then
        begin
          A0X := a[AOffset + 0];
          A1X := a[AOffset + 1];
          A2X := a[AOffset + 2];
          A3X := a[AOffset + 3];
          T1X := A0X + A2X;
          T2X := A1X + A3X;
          M2X := A0X - A2X;
          M3Y := A3X - A1X;
          a[AOffset + 0] := T1X + T2X;
          a[AOffset + 1] := M2X - M3Y;
          a[AOffset + 2] := T1X - T2X;
          a[AOffset + 3] := M2X + M3Y;
          Exit;
        end;
      if n = 5 then
        begin
          Offs := Plan.Plan[EntryOffset + 7];
          c1 := Plan.Precomputed[Offs + 0];
          c2 := Plan.Precomputed[Offs + 1];
          c3 := Plan.Precomputed[Offs + 2];
          c4 := Plan.Precomputed[Offs + 3];
          C5 := Plan.Precomputed[Offs + 4];
          T1X := a[AOffset + 1] + a[AOffset + 4];
          T2X := a[AOffset + 2] + a[AOffset + 3];
          T3X := a[AOffset + 1] - a[AOffset + 4];
          T4X := a[AOffset + 3] - a[AOffset + 2];
          T5X := T1X + T2X;
          v0 := a[AOffset + 0] + T5X;
          a[AOffset + 0] := v0;
          M2X := c2 * (T1X - T2X);
          M3Y := c3 * (T3X + T4X);
          S3Y := M3Y - c4 * T4X;
          S5Y := M3Y + C5 * T3X;
          S1X := v0 + c1 * T5X;
          S2X := S1X + M2X;
          S4X := S1X - M2X;
          a[AOffset + 1] := S2X - S3Y;
          a[AOffset + 2] := S4X - S5Y;
          a[AOffset + 3] := S4X + S5Y;
          a[AOffset + 4] := S2X + S3Y;
          Exit;
        end;
    end;
  if Plan.Plan[EntryOffset + 3] = FFTBluesteinPlan then
    begin

      //
      // Bluestein plan:
      // 1. multiply by precomputed coefficients
      // 2. make convolution: forward FFT, multiplication by precomputed FFT
      // and backward FFT. backward FFT is represented as
      //
      // invfft(x) = fft(x')'/M
      //
      // for performance reasons reduction of inverse FFT to
      // forward FFT is merged with multiplication of FFT components
      // and last stage of Bluestein's transformation.
      // 3. post-multiplication by Bluestein factors
      //
      n := Plan.Plan[EntryOffset + 1];
      M := Plan.Plan[EntryOffset + 4];
      Offs := Plan.Plan[EntryOffset + 7];
      i := StackPtr + 2 * n;
      while i <= StackPtr + 2 * M - 1 do
        begin
          Plan.StackBuf[i] := 0;
          inc(i);
        end;
      OffsP := Offs + 2 * M;
      OffsA := AOffset;
      OffsB := StackPtr;
      i := 0;
      while i <= n - 1 do
        begin
          Bx := Plan.Precomputed[OffsP + 0];
          By := Plan.Precomputed[OffsP + 1];
          x := a[OffsA + 0];
          y := a[OffsA + 1];
          Plan.StackBuf[OffsB + 0] := x * Bx - y * -By;
          Plan.StackBuf[OffsB + 1] := x * -By + y * Bx;
          OffsP := OffsP + 2;
          OffsA := OffsA + 2;
          OffsB := OffsB + 2;
          inc(i);
        end;
      FTBaseExecutePlanRec(Plan.StackBuf, StackPtr, Plan,
        Plan.Plan[EntryOffset + 5], StackPtr + 2 * 2 * M);
      OffsB := StackPtr;
      OffsP := Offs;
      i := 0;
      while i <= M - 1 do
        begin
          x := Plan.StackBuf[OffsB + 0];
          y := Plan.StackBuf[OffsB + 1];
          Bx := Plan.Precomputed[OffsP + 0];
          By := Plan.Precomputed[OffsP + 1];
          Plan.StackBuf[OffsB + 0] := x * Bx - y * By;
          Plan.StackBuf[OffsB + 1] := -(x * By + y * Bx);
          OffsB := OffsB + 2;
          OffsP := OffsP + 2;
          inc(i);
        end;
      FTBaseExecutePlanRec(Plan.StackBuf, StackPtr, Plan,
        Plan.Plan[EntryOffset + 5], StackPtr + 2 * 2 * M);
      OffsB := StackPtr;
      OffsP := Offs + 2 * M;
      OffsA := AOffset;
      i := 0;
      while i <= n - 1 do
        begin
          x := +Plan.StackBuf[OffsB + 0] / M;
          y := -Plan.StackBuf[OffsB + 1] / M;
          Bx := Plan.Precomputed[OffsP + 0];
          By := Plan.Precomputed[OffsP + 1];
          a[OffsA + 0] := x * Bx - y * -By;
          a[OffsA + 1] := x * -By + y * Bx;
          OffsP := OffsP + 2;
          OffsA := OffsA + 2;
          OffsB := OffsB + 2;
          inc(i);
        end;
      Exit;
    end;
end;

(* ************************************************************************
  Returns good factorization N=N1*N2.

  Usually N1<=N2 (but not always - small N's may be exception).
  if N1<>1 then N2<>1.

  Factorization is chosen depending on task type and codelets we have.
  ************************************************************************ *)
procedure FTBaseFactorize(n: TLInt; TaskType: TLInt;
  var n1: TLInt; var n2: TLInt);
var
  j: TLInt;
begin
  n1 := 0;
  n2 := 0;

  //
  // try to find good codelet
  //
  if n1 * n2 <> n then
    begin
      j := FTBaseCodeletRecommended;
      while j >= 2 do
        begin
          if n mod j = 0 then
            begin
              n1 := j;
              n2 := n div j;
              Break;
            end;
          dec(j);
        end;
    end;

  //
  // try to factorize N
  //
  if n1 * n2 <> n then
    begin
      j := FTBaseCodeletRecommended + 1;
      while j <= n - 1 do
        begin
          if n mod j = 0 then
            begin
              n1 := j;
              n2 := n div j;
              Break;
            end;
          inc(j);
        end;
    end;

  //
  // looks like N is prime :(
  //
  if n1 * n2 <> n then
    begin
      n1 := 1;
      n2 := n;
    end;

  //
  // normalize
  //
  if (n2 = 1) and (n1 <> 1) then
    begin
      n2 := n1;
      n1 := 1;
    end;
end;

(* ************************************************************************
  Is number smooth?
  ************************************************************************ *)
function FTBaseIsSmooth(n: TLInt): Boolean;
var
  i: TLInt;
begin
  i := 2;
  while i <= FTBaseMaxSmoothFactor do
    begin
      while n mod i = 0 do
        begin
          n := n div i;
        end;
      inc(i);
    end;
  Result := n = 1;
end;

(* ************************************************************************
  Returns smallest smooth (divisible only by 2, 3, 5) number that is greater
  than or equal to max(N,2)
  ************************************************************************ *)
function FTBaseFindSmooth(n: TLInt): TLInt;
var
  Best: TLInt;
begin
  Best := 2;
  while Best < n do
    begin
      Best := 2 * Best;
    end;
  FTBaseFindSmoothRec(n, 1, 2, Best);
  Result := Best;
end;

(* ************************************************************************
  Returns  smallest  smooth  (divisible only by 2, 3, 5) even number that is
  greater than or equal to max(N,2)
  ************************************************************************ *)
function FTBaseFindSmoothEven(n: TLInt): TLInt;
var
  Best: TLInt;
begin
  Best := 2;
  while Best < n do
    begin
      Best := 2 * Best;
    end;
  FTBaseFindSmoothRec(n, 2, 2, Best);
  Result := Best;
end;

(* ************************************************************************
  Returns estimate of FLOP count for the FFT.

  It is only an estimate based on operations count for the PERFECT FFT
  and relative inefficiency of the algorithm actually used.

  N should be power of 2, estimates are badly wrong for non-power-of-2 N's.
  ************************************************************************ *)
function FTBaseGetFLOPEstimate(n: TLInt): TLFloat;
begin
  Result := FTBaseInefficiencyFactor * (4 * n * ln(n) / ln(2) - 6 * n + 8);
end;

(* ************************************************************************
  Recurrent subroutine for the FFTGeneratePlan:

  PARAMETERS:
  N                   plan size
  IsReal              whether input is real or not.
  subroutine MUST NOT ignore this flag because real
  inputs comes with non-initialized imaginary parts,
  so ignoring this flag will result in corrupted output
  HalfOut             whether full output or only half of it from 0 to
  floor(N/2) is needed. This flag may be ignored if
  doing so will simplify calculations
  Plan                plan array
  PlanSize            size of used part (in integers)
  PrecomputedSize     size of precomputed array allocated yet
  PlanArraySize       plan array size (actual)
  TmpMemSize          temporary memory required size
  BluesteinMemSize    temporary memory required size
  ************************************************************************ *)
procedure FTBaseGeneratePlanRec(n: TLInt; TaskType: TLInt;
  var Plan: TFTPlan; var PlanSize: TLInt;
  var PrecomputedSize: TLInt; var PlanArraySize: TLInt;
  var TmpMemSize: TLInt; var StackMemSize: TLInt;
  StackPtr: TLInt);
var
  k: TLInt;
  M: TLInt;
  n1: TLInt;
  n2: TLInt;
  ESize: TLInt;
  EntryOffset: TLInt;
begin

  //
  // prepare
  //
  if PlanSize + FTBasePlanEntrySize > PlanArraySize then
    begin
      FFTArrayResize(Plan.Plan, PlanArraySize, 8 * PlanArraySize);
    end;
  EntryOffset := PlanSize;
  ESize := FTBasePlanEntrySize;
  PlanSize := PlanSize + ESize;

  //
  // if N=1, generate empty plan and exit
  //
  if n = 1 then
    begin
      Plan.Plan[EntryOffset + 0] := ESize;
      Plan.Plan[EntryOffset + 1] := -1;
      Plan.Plan[EntryOffset + 2] := -1;
      Plan.Plan[EntryOffset + 3] := FFTEmptyPlan;
      Plan.Plan[EntryOffset + 4] := -1;
      Plan.Plan[EntryOffset + 5] := -1;
      Plan.Plan[EntryOffset + 6] := -1;
      Plan.Plan[EntryOffset + 7] := -1;
      Exit;
    end;

  //
  // generate plans
  //
  FTBaseFactorize(n, TaskType, n1, n2);
  if (TaskType = FTBaseCFFTTask) or (TaskType = FTBaseRFFTTask) then
    begin

      //
      // TLComplex FFT plans
      //
      if n1 <> 1 then
        begin

          //
          // Cooley-Tukey plan (real or TLComplex)
          //
          // Note that child plans are TLComplex
          // (whether plan itself is TLComplex or not).
          //
          TmpMemSize := Max(TmpMemSize, 2 * n1 * n2);
          Plan.Plan[EntryOffset + 0] := ESize;
          Plan.Plan[EntryOffset + 1] := n1;
          Plan.Plan[EntryOffset + 2] := n2;
          if TaskType = FTBaseCFFTTask then
            begin
              Plan.Plan[EntryOffset + 3] := FFTCooleyTukeyPlan;
            end
          else
            begin
              Plan.Plan[EntryOffset + 3] := FFTRealCooleyTukeyPlan;
            end;
          Plan.Plan[EntryOffset + 4] := 0;
          Plan.Plan[EntryOffset + 5] := PlanSize;
          FTBaseGeneratePlanRec(n1, FTBaseCFFTTask, Plan, PlanSize, PrecomputedSize,
            PlanArraySize, TmpMemSize, StackMemSize, StackPtr);
          Plan.Plan[EntryOffset + 6] := PlanSize;
          FTBaseGeneratePlanRec(n2, FTBaseCFFTTask, Plan, PlanSize, PrecomputedSize,
            PlanArraySize, TmpMemSize, StackMemSize, StackPtr);
          Plan.Plan[EntryOffset + 7] := -1;
          Exit;
        end
      else
        begin
          if (n = 2) or (n = 3) or (n = 4) or (n = 5) then
            begin

              //
              // hard-coded plan
              //
              Plan.Plan[EntryOffset + 0] := ESize;
              Plan.Plan[EntryOffset + 1] := n1;
              Plan.Plan[EntryOffset + 2] := n2;
              Plan.Plan[EntryOffset + 3] := FFTCodeletPlan;
              Plan.Plan[EntryOffset + 4] := 0;
              Plan.Plan[EntryOffset + 5] := -1;
              Plan.Plan[EntryOffset + 6] := -1;
              Plan.Plan[EntryOffset + 7] := PrecomputedSize;
              if n = 3 then
                begin
                  PrecomputedSize := PrecomputedSize + 2;
                end;
              if n = 5 then
                begin
                  PrecomputedSize := PrecomputedSize + 5;
                end;
              Exit;
            end
          else
            begin

              //
              // Bluestein's plan
              //
              // Select such M that M>=2*N-1, M is composite, and M's
              // factors are 2, 3, 5
              //
              k := 2 * n2 - 1;
              M := FTBaseFindSmooth(k);
              TmpMemSize := Max(TmpMemSize, 2 * M);
              Plan.Plan[EntryOffset + 0] := ESize;
              Plan.Plan[EntryOffset + 1] := n2;
              Plan.Plan[EntryOffset + 2] := -1;
              Plan.Plan[EntryOffset + 3] := FFTBluesteinPlan;
              Plan.Plan[EntryOffset + 4] := M;
              Plan.Plan[EntryOffset + 5] := PlanSize;
              StackPtr := StackPtr + 2 * 2 * M;
              StackMemSize := Max(StackMemSize, StackPtr);
              FTBaseGeneratePlanRec(M, FTBaseCFFTTask, Plan, PlanSize,
                PrecomputedSize, PlanArraySize, TmpMemSize, StackMemSize, StackPtr);
              StackPtr := StackPtr - 2 * 2 * M;
              Plan.Plan[EntryOffset + 6] := -1;
              Plan.Plan[EntryOffset + 7] := PrecomputedSize;
              PrecomputedSize := PrecomputedSize + 2 * M + 2 * n;
              Exit;
            end;
        end;
    end;
  if TaskType = FTBaseRFHTTask then
    begin

      //
      // real FHT plans
      //
      if n1 <> 1 then
        begin

          //
          // Cooley-Tukey plan
          //
          //
          TmpMemSize := Max(TmpMemSize, 2 * n1 * n2);
          Plan.Plan[EntryOffset + 0] := ESize;
          Plan.Plan[EntryOffset + 1] := n1;
          Plan.Plan[EntryOffset + 2] := n2;
          Plan.Plan[EntryOffset + 3] := FHTCooleyTukeyPlan;
          Plan.Plan[EntryOffset + 4] := 0;
          Plan.Plan[EntryOffset + 5] := PlanSize;
          FTBaseGeneratePlanRec(n1, TaskType, Plan, PlanSize, PrecomputedSize,
            PlanArraySize, TmpMemSize, StackMemSize, StackPtr);
          Plan.Plan[EntryOffset + 6] := PlanSize;
          FTBaseGeneratePlanRec(n2, TaskType, Plan, PlanSize, PrecomputedSize,
            PlanArraySize, TmpMemSize, StackMemSize, StackPtr);
          Plan.Plan[EntryOffset + 7] := -1;
          Exit;
        end
      else
        begin

          //
          // N2 plan
          //
          Plan.Plan[EntryOffset + 0] := ESize;
          Plan.Plan[EntryOffset + 1] := n1;
          Plan.Plan[EntryOffset + 2] := n2;
          Plan.Plan[EntryOffset + 3] := FHTN2Plan;
          Plan.Plan[EntryOffset + 4] := 0;
          Plan.Plan[EntryOffset + 5] := -1;
          Plan.Plan[EntryOffset + 6] := -1;
          Plan.Plan[EntryOffset + 7] := -1;
          if (n = 2) or (n = 3) or (n = 4) or (n = 5) then
            begin

              //
              // hard-coded plan
              //
              Plan.Plan[EntryOffset + 0] := ESize;
              Plan.Plan[EntryOffset + 1] := n1;
              Plan.Plan[EntryOffset + 2] := n2;
              Plan.Plan[EntryOffset + 3] := FHTCodeletPlan;
              Plan.Plan[EntryOffset + 4] := 0;
              Plan.Plan[EntryOffset + 5] := -1;
              Plan.Plan[EntryOffset + 6] := -1;
              Plan.Plan[EntryOffset + 7] := PrecomputedSize;
              if n = 3 then
                begin
                  PrecomputedSize := PrecomputedSize + 2;
                end;
              if n = 5 then
                begin
                  PrecomputedSize := PrecomputedSize + 5;
                end;
              Exit;
            end;
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Recurrent subroutine for precomputing FFT plans
  ************************************************************************ *)
procedure FTBasePrecomputePlanRec(var Plan: TFTPlan; EntryOffset: TLInt; StackPtr: TLInt);
var
  i: TLInt;
  idx: TLInt;
  n1: TLInt;
  n2: TLInt;
  n: TLInt;
  M: TLInt;
  Offs: TLInt;
  v: TLFloat;
  EmptyArray: TLVec;
  Bx: TLFloat;
  By: TLFloat;
begin
  if (Plan.Plan[EntryOffset + 3] = FFTCooleyTukeyPlan) or
    (Plan.Plan[EntryOffset + 3] = FFTRealCooleyTukeyPlan) or
    (Plan.Plan[EntryOffset + 3] = FHTCooleyTukeyPlan) then
    begin
      FTBasePrecomputePlanRec(Plan, Plan.Plan[EntryOffset + 5], StackPtr);
      FTBasePrecomputePlanRec(Plan, Plan.Plan[EntryOffset + 6], StackPtr);
      Exit;
    end;
  if (Plan.Plan[EntryOffset + 3] = FFTCodeletPlan) or
    (Plan.Plan[EntryOffset + 3] = FHTCodeletPlan) then
    begin
      n1 := Plan.Plan[EntryOffset + 1];
      n2 := Plan.Plan[EntryOffset + 2];
      n := n1 * n2;
      if n = 3 then
        begin
          Offs := Plan.Plan[EntryOffset + 7];
          Plan.Precomputed[Offs + 0] := Cos(2 * pi / 3) - 1;
          Plan.Precomputed[Offs + 1] := Sin(2 * pi / 3);
          Exit;
        end;
      if n = 5 then
        begin
          Offs := Plan.Plan[EntryOffset + 7];
          v := 2 * pi / 5;
          Plan.Precomputed[Offs + 0] := (Cos(v) + Cos(2 * v)) / 2 - 1;
          Plan.Precomputed[Offs + 1] := (Cos(v) - Cos(2 * v)) / 2;
          Plan.Precomputed[Offs + 2] := -Sin(v);
          Plan.Precomputed[Offs + 3] := -(Sin(v) + Sin(2 * v));
          Plan.Precomputed[Offs + 4] := Sin(v) - Sin(2 * v);
          Exit;
        end;
    end;
  if Plan.Plan[EntryOffset + 3] = FFTBluesteinPlan then
    begin
      FTBasePrecomputePlanRec(Plan, Plan.Plan[EntryOffset + 5], StackPtr);
      n := Plan.Plan[EntryOffset + 1];
      M := Plan.Plan[EntryOffset + 4];
      Offs := Plan.Plan[EntryOffset + 7];
      i := 0;
      while i <= 2 * M - 1 do
        begin
          Plan.Precomputed[Offs + i] := 0;
          inc(i);
        end;
      i := 0;
      while i <= n - 1 do
        begin
          Bx := Cos(pi * AP_Sqr(i) / n);
          By := Sin(pi * AP_Sqr(i) / n);
          Plan.Precomputed[Offs + 2 * i + 0] := Bx;
          Plan.Precomputed[Offs + 2 * i + 1] := By;
          Plan.Precomputed[Offs + 2 * M + 2 * i + 0] := Bx;
          Plan.Precomputed[Offs + 2 * M + 2 * i + 1] := By;
          if i > 0 then
            begin
              Plan.Precomputed[Offs + 2 * (M - i) + 0] := Bx;
              Plan.Precomputed[Offs + 2 * (M - i) + 1] := By;
            end;
          inc(i);
        end;
      FTBaseExecutePlanRec(Plan.Precomputed, Offs, Plan,
        Plan.Plan[EntryOffset + 5], StackPtr);
      Exit;
    end;
end;

(* ************************************************************************
  Twiddle factors calculation
  ************************************************************************ *)
procedure FFTTwCalc(var a: TLVec; AOffset: TLInt; n1: TLInt; n2: TLInt);
var
  i: TLInt;
  j: TLInt;
  n: TLInt;
  idx: TLInt;
  Offs: TLInt;
  x: TLFloat;
  y: TLFloat;
  TwXM1: TLFloat;
  TwY: TLFloat;
  TwBaseXM1: TLFloat;
  TwBaseY: TLFloat;
  TwRowXM1: TLFloat;
  TwRowY: TLFloat;
  TmpX: TLFloat;
  TmpY: TLFloat;
  v: TLFloat;
begin
  n := n1 * n2;
  v := -2 * pi / n;
  TwBaseXM1 := -2 * AP_Sqr(Sin(0.5 * v));
  TwBaseY := Sin(v);
  TwRowXM1 := 0;
  TwRowY := 0;
  i := 0;
  while i <= n2 - 1 do
    begin
      TwXM1 := 0;
      TwY := 0;
      j := 0;
      while j <= n1 - 1 do
        begin
          idx := i * n1 + j;
          Offs := AOffset + 2 * idx;
          x := a[Offs + 0];
          y := a[Offs + 1];
          TmpX := x * TwXM1 - y * TwY;
          TmpY := x * TwY + y * TwXM1;
          a[Offs + 0] := x + TmpX;
          a[Offs + 1] := y + TmpY;

          //
          // update Tw: Tw(new) = Tw(old)*TwRow
          //
          if j < n1 - 1 then
            begin
              if j mod FTBaseUpdateTw = 0 then
                begin
                  v := -2 * pi * i * (j + 1) / n;
                  TwXM1 := -2 * AP_Sqr(Sin(0.5 * v));
                  TwY := Sin(v);
                end
              else
                begin
                  TmpX := TwRowXM1 + TwXM1 * TwRowXM1 - TwY * TwRowY;
                  TmpY := TwRowY + TwXM1 * TwRowY + TwY * TwRowXM1;
                  TwXM1 := TwXM1 + TmpX;
                  TwY := TwY + TmpY;
                end;
            end;
          inc(j);
        end;

      //
      // update TwRow: TwRow(new) = TwRow(old)*TwBase
      //
      if i < n2 - 1 then
        begin
          if j mod FTBaseUpdateTw = 0 then
            begin
              v := -2 * pi * (i + 1) / n;
              TwRowXM1 := -2 * AP_Sqr(Sin(0.5 * v));
              TwRowY := Sin(v);
            end
          else
            begin
              TmpX := TwBaseXM1 + TwRowXM1 * TwBaseXM1 - TwRowY * TwBaseY;
              TmpY := TwBaseY + TwRowXM1 * TwBaseY + TwRowY * TwBaseXM1;
              TwRowXM1 := TwRowXM1 + TmpX;
              TwRowY := TwRowY + TmpY;
            end;
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Linear transpose: transpose TLComplex matrix stored in 1-dimensional array
  ************************************************************************ *)
procedure InternalComplexLinTranspose(var a: TLVec; M: TLInt; n: TLInt; AStart: TLInt; var Buf: TLVec);
begin
  FFTICLTRec(a, AStart, n, Buf, 0, M, M, n);
  APVMove(@a[0], AStart, AStart + 2 * M * n - 1, @Buf[0], 0, 2 * M * n - 1);
end;

(* ************************************************************************
  Linear transpose: transpose real matrix stored in 1-dimensional array
  ************************************************************************ *)
procedure InternalRealLinTranspose(var a: TLVec; M: TLInt; n: TLInt; AStart: TLInt; var Buf: TLVec);
begin
  FFTIRLTRec(a, AStart, n, Buf, 0, M, M, n);
  APVMove(@a[0], AStart, AStart + M * n - 1, @Buf[0], 0, M * n - 1);
end;

(* ************************************************************************
  Recurrent subroutine for a InternalComplexLinTranspose

  Write A^T to B, where:
  * A is m*n TLComplex matrix stored in array A as pairs of real/image values,
  beginning from AStart position, with AStride stride
  * B is n*m TLComplex matrix stored in array B as pairs of real/image values,
  beginning from BStart position, with BStride stride
  stride is measured in TLComplex numbers, i.e. in real/image pairs.
  ************************************************************************ *)
procedure FFTICLTRec(var a: TLVec; AStart: TLInt; AStride: TLInt; var b: TLVec; BStart: TLInt; BStride: TLInt; M: TLInt; n: TLInt);
var
  i: TLInt;
  j: TLInt;
  idx1: TLInt;
  idx2: TLInt;
  m2: TLInt;
  m1: TLInt;
  n1: TLInt;
begin
  if (M = 0) or (n = 0) then
    begin
      Exit;
    end;
  if Max(M, n) <= 8 then
    begin
      m2 := 2 * BStride;
      i := 0;
      while i <= M - 1 do
        begin
          idx1 := BStart + 2 * i;
          idx2 := AStart + 2 * i * AStride;
          j := 0;
          while j <= n - 1 do
            begin
              b[idx1 + 0] := a[idx2 + 0];
              b[idx1 + 1] := a[idx2 + 1];
              idx1 := idx1 + m2;
              idx2 := idx2 + 2;
              inc(j);
            end;
          inc(i);
        end;
      Exit;
    end;
  if n > M then
    begin

      //
      // New partition:
      //
      // "A^T -> B" becomes "(A1 A2)^T -> ( B1 )
      // ( B2 )
      //
      n1 := n div 2;
      if (n - n1 >= 8) and (n1 mod 8 <> 0) then
        begin
          n1 := n1 + (8 - n1 mod 8);
        end;
      Assert(n - n1 > 0);
      FFTICLTRec(a, AStart, AStride, b, BStart, BStride, M, n1);
      FFTICLTRec(a, AStart + 2 * n1, AStride, b, BStart + 2 * n1 * BStride,
        BStride, M, n - n1);
    end
  else
    begin

      //
      // New partition:
      //
      // "A^T -> B" becomes "( A1 )^T -> ( B1 B2 )
      // ( A2 )
      //
      m1 := M div 2;
      if (M - m1 >= 8) and (m1 mod 8 <> 0) then
        begin
          m1 := m1 + (8 - m1 mod 8);
        end;
      Assert(M - m1 > 0);
      FFTICLTRec(a, AStart, AStride, b, BStart, BStride, m1, n);
      FFTICLTRec(a, AStart + 2 * m1 * AStride, AStride, b, BStart + 2 * m1,
        BStride, M - m1, n);
    end;
end;

(* ************************************************************************
  Recurrent subroutine for a InternalRealLinTranspose
  ************************************************************************ *)
procedure FFTIRLTRec(var a: TLVec; AStart: TLInt; AStride: TLInt; var b: TLVec; BStart: TLInt; BStride: TLInt; M: TLInt; n: TLInt);
var
  i: TLInt;
  j: TLInt;
  idx1: TLInt;
  idx2: TLInt;
  m1: TLInt;
  n1: TLInt;
begin
  if (M = 0) or (n = 0) then
    begin
      Exit;
    end;
  if Max(M, n) <= 8 then
    begin
      i := 0;
      while i <= M - 1 do
        begin
          idx1 := BStart + i;
          idx2 := AStart + i * AStride;
          j := 0;
          while j <= n - 1 do
            begin
              b[idx1] := a[idx2];
              idx1 := idx1 + BStride;
              idx2 := idx2 + 1;
              inc(j);
            end;
          inc(i);
        end;
      Exit;
    end;
  if n > M then
    begin

      //
      // New partition:
      //
      // "A^T -> B" becomes "(A1 A2)^T -> ( B1 )
      // ( B2 )
      //
      n1 := n div 2;
      if (n - n1 >= 8) and (n1 mod 8 <> 0) then
        begin
          n1 := n1 + (8 - n1 mod 8);
        end;
      Assert(n - n1 > 0);
      FFTIRLTRec(a, AStart, AStride, b, BStart, BStride, M, n1);
      FFTIRLTRec(a, AStart + n1, AStride, b, BStart + n1 * BStride, BStride,
        M, n - n1);
    end
  else
    begin

      //
      // New partition:
      //
      // "A^T -> B" becomes "( A1 )^T -> ( B1 B2 )
      // ( A2 )
      //
      m1 := M div 2;
      if (M - m1 >= 8) and (m1 mod 8 <> 0) then
        begin
          m1 := m1 + (8 - m1 mod 8);
        end;
      Assert(M - m1 > 0);
      FFTIRLTRec(a, AStart, AStride, b, BStart, BStride, m1, n);
      FFTIRLTRec(a, AStart + m1 * AStride, AStride, b, BStart + m1, BStride,
        M - m1, n);
    end;
end;

(* ************************************************************************
  recurrent subroutine for FFTFindSmoothRec
  ************************************************************************ *)
procedure FTBaseFindSmoothRec(n: TLInt; Seed: TLInt; LeastFactor: TLInt; var Best: TLInt);
begin
  Assert(FTBaseMaxSmoothFactor <= 5, 'FTBaseFindSmoothRec: internal error!');
  if Seed >= n then
    begin
      Best := Min(Best, Seed);
      Exit;
    end;
  if LeastFactor <= 2 then
    begin
      FTBaseFindSmoothRec(n, Seed * 2, 2, Best);
    end;
  if LeastFactor <= 3 then
    begin
      FTBaseFindSmoothRec(n, Seed * 3, 3, Best);
    end;
  if LeastFactor <= 5 then
    begin
      FTBaseFindSmoothRec(n, Seed * 5, 5, Best);
    end;
end;

(* ************************************************************************
  Internal subroutine: array resize
  ************************************************************************ *)
procedure FFTArrayResize(var a: TLIVec; var ASize: TLInt; NewASize: TLInt);
var
  tmp: TLIVec;
  i: TLInt;
begin
  SetLength(tmp, ASize);
  i := 0;
  while i <= ASize - 1 do
    begin
      tmp[i] := a[i];
      inc(i);
    end;
  SetLength(a, NewASize);
  i := 0;
  while i <= ASize - 1 do
    begin
      a[i] := tmp[i];
      inc(i);
    end;
  ASize := NewASize;
end;

(* ************************************************************************
  Reference FHT stub
  ************************************************************************ *)
procedure RefFHT(var a: TLVec; n: TLInt; Offs: TLInt);
var
  Buf: TLVec;
  i: TLInt;
  j: TLInt;
  v: TLFloat;
begin
  Assert(n > 0, 'RefFHTR1D: incorrect N!');
  SetLength(Buf, n);
  i := 0;
  while i <= n - 1 do
    begin
      v := 0;
      j := 0;
      while j <= n - 1 do
        begin
          v := v + a[Offs + j] *
            (Cos(2 * pi * i * j / n) + Sin(2 * pi * i * j / n));
          inc(j);
        end;
      Buf[i] := v;
      inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      a[Offs + i] := Buf[i];
      inc(i);
    end;
end;

(* ************************************************************************
  1-dimensional TLComplex FFT.

  Array size N may be arbitrary number (composite or prime).  Composite  N's
  are handled with cache-oblivious variation of  a  Cooley-Tukey  algorithm.
  Small prime-factors are transformed using hard coded  codelets (similar to
  FFTW codelets, but without low-level  optimization),  large  prime-factors
  are handled with Bluestein's algorithm.

  Fastests transforms are for smooth N's (prime factors are 2, 3,  5  only),
  most fast for powers of 2. When N have prime factors  larger  than  these,
  but orders of magnitude smaller than N, computations will be about 4 times
  slower than for nearby highly composite N's. When N itself is prime, speed
  will be 6 times lower.

  Algorithm has O(N*logN) complexity for any N (composite or prime).

  INPUT PARAMETERS
  A   -   array[0..N-1] - TLComplex function to be transformed
  N   -   problem size

  OUTPUT PARAMETERS
  A   -   DFT of a input array, array[0..N-1]
  A_out[j] = SUM(A_in[k]*exp(-2*pi*sqrt(-1)*j*k/N), k = 0..N-1)
  ************************************************************************ *)
procedure FFTC1D(var a: TLComplexVec; n: TLInt);
var
  Plan: TFTPlan;
  i: TLInt;
  Buf: TLVec;
begin
  Assert(n > 0, 'FFTC1D: incorrect N!');

  //
  // Special case: N=1, FFT is just identity transform.
  // After this block we assume that N is strictly greater than 1.
  //
  if n = 1 then
    begin
      Exit;
    end;

  //
  // convert input array to the more convinient format
  //
  SetLength(Buf, 2 * n);
  i := 0;
  while i <= n - 1 do
    begin
      Buf[2 * i + 0] := a[i].x;
      Buf[2 * i + 1] := a[i].y;
      inc(i);
    end;

  //
  // Generate plan and execute it.
  //
  // Plan is a combination of a successive factorizations of N and
  // precomputed data. It is much like a FFTW plan, but is not stored
  // between subroutine calls and is much simpler.
  //
  FTBaseGenerateComplexFFTPlan(n, Plan);
  FTBaseExecutePlan(Buf, 0, n, Plan);

  //
  // result
  //
  i := 0;
  while i <= n - 1 do
    begin
      a[i].x := Buf[2 * i + 0];
      a[i].y := Buf[2 * i + 1];
      inc(i);
    end;
end;

(* ************************************************************************
  1-dimensional TLComplex inverse FFT.

  Array size N may be arbitrary number (composite or prime).  Algorithm  has
  O(N*logN) complexity for any N (composite or prime).

  See FFTC1D() description for more information about algorithm performance.

  INPUT PARAMETERS
  A   -   array[0..N-1] - TLComplex array to be transformed
  N   -   problem size

  OUTPUT PARAMETERS
  A   -   inverse DFT of a input array, array[0..N-1]
  A_out[j] = SUM(A_in[k]/N*exp(+2*pi*sqrt(-1)*j*k/N), k = 0..N-1)
  ************************************************************************ *)
procedure FFTC1DInv(var a: TLComplexVec; n: TLInt);
var
  i: TLInt;
begin
  Assert(n > 0, 'FFTC1DInv: incorrect N!');

  //
  // Inverse DFT can be expressed in terms of the DFT as
  //
  // invfft(x) = fft(x')'/N
  //
  // here x' means conj(x).
  //
  i := 0;
  while i <= n - 1 do
    begin
      a[i].y := -a[i].y;
      inc(i);
    end;
  FFTC1D(a, n);
  i := 0;
  while i <= n - 1 do
    begin
      a[i].x := a[i].x / n;
      a[i].y := -a[i].y / n;
      inc(i);
    end;
end;

(* ************************************************************************
  1-dimensional real FFT.

  Algorithm has O(N*logN) complexity for any N (composite or prime).

  INPUT PARAMETERS
  A   -   array[0..N-1] - real function to be transformed
  N   -   problem size

  OUTPUT PARAMETERS
  F   -   DFT of a input array, array[0..N-1]
  F[j] = SUM(A[k]*exp(-2*pi*sqrt(-1)*j*k/N), k = 0..N-1)

  NOTE:
  F[] satisfies symmetry property F[k] = conj(F[N-k]),  so just one half
  of  array  is  usually needed. But for convinience subroutine returns full
  TLComplex array (with frequencies above N/2), so its result may be  used  by
  other FFT-related subroutines.
  ************************************************************************ *)
procedure FFTR1D(const a: TLVec; n: TLInt; var f: TLComplexVec);
var
  i: TLInt;
  n2: TLInt;
  idx: TLInt;
  Hn: TLComplex;
  HmnC: TLComplex;
  v: TLComplex;
  Buf: TLVec;
  Plan: TFTPlan;
begin
  Assert(n > 0, 'FFTR1D: incorrect N!');

  //
  // Special cases:
  // * N=1, FFT is just identity transform.
  // * N=2, FFT is simple too
  //
  // After this block we assume that N is strictly greater than 2
  //
  if n = 1 then
    begin
      SetLength(f, 1);
      f[0] := C_Complex(a[0]);
      Exit;
    end;
  if n = 2 then
    begin
      SetLength(f, 2);
      f[0].x := a[0] + a[1];
      f[0].y := 0;
      f[1].x := a[0] - a[1];
      f[1].y := 0;
      Exit;
    end;

  //
  // Choose between odd-size and even-size FFTs
  //
  if n mod 2 = 0 then
    begin

      //
      // even-size real FFT, use reduction to the TLComplex task
      //
      n2 := n div 2;
      SetLength(Buf, n);
      APVMove(@Buf[0], 0, n - 1, @a[0], 0, n - 1);
      FTBaseGenerateComplexFFTPlan(n2, Plan);
      FTBaseExecutePlan(Buf, 0, n2, Plan);
      SetLength(f, n);
      i := 0;
      while i <= n2 do
        begin
          idx := 2 * (i mod n2);
          Hn.x := Buf[idx + 0];
          Hn.y := Buf[idx + 1];
          idx := 2 * ((n2 - i) mod n2);
          HmnC.x := Buf[idx + 0];
          HmnC.y := -Buf[idx + 1];
          v.x := -Sin(-2 * pi * i / n);
          v.y := Cos(-2 * pi * i / n);
          f[i] := C_Sub(C_Add(Hn, HmnC), C_Mul(v, C_Sub(Hn, HmnC)));
          f[i].x := 0.5 * f[i].x;
          f[i].y := 0.5 * f[i].y;
          inc(i);
        end;
      i := n2 + 1;
      while i <= n - 1 do
        begin
          f[i] := Conj(f[n - i]);
          inc(i);
        end;
      Exit;
    end
  else
    begin

      //
      // use TLComplex FFT
      //
      SetLength(f, n);
      i := 0;
      while i <= n - 1 do
        begin
          f[i] := C_Complex(a[i]);
          inc(i);
        end;
      FFTC1D(f, n);
      Exit;
    end;
end;

(* ************************************************************************
  1-dimensional real inverse FFT.

  Algorithm has O(N*logN) complexity for any N (composite or prime).

  INPUT PARAMETERS
  F   -   array[0..floor(N/2)] - frequencies from forward real FFT
  N   -   problem size

  OUTPUT PARAMETERS
  A   -   inverse DFT of a input array, array[0..N-1]

  NOTE:
  F[] should satisfy symmetry property F[k] = conj(F[N-k]), so just  one
  half of frequencies array is needed - elements from 0 to floor(N/2).  F[0]
  is ALWAYS real. If N is even F[floor(N/2)] is real too. If N is odd,  then
  F[floor(N/2)] has no special properties.

  Relying on properties noted above, FFTR1DInv subroutine uses only elements
  from 0th to floor(N/2)-th. It ignores imaginary part of F[0],  and in case
  N is even it ignores imaginary part of F[floor(N/2)] too.  So you can pass
  either frequencies array with N elements or reduced array with roughly N/2
  elements - subroutine will successfully transform both.
  ************************************************************************ *)
procedure FFTR1DInv(const f: TLComplexVec; n: TLInt; var a: TLVec);
var
  i: TLInt;
  h: TLVec;
  FH: TLComplexVec;
begin
  Assert(n > 0, 'FFTR1DInv: incorrect N!');

  //
  // Special case: N=1, FFT is just identity transform.
  // After this block we assume that N is strictly greater than 1.
  //
  if n = 1 then
    begin
      SetLength(a, 1);
      a[0] := f[0].x;
      Exit;
    end;

  //
  // inverse real FFT is reduced to the inverse real FHT,
  // which is reduced to the forward real FHT,
  // which is reduced to the forward real FFT.
  //
  // Don't worry, it is really compact and efficient reduction :)
  //
  SetLength(h, n);
  SetLength(a, n);
  h[0] := f[0].x;
  i := 1;
  while i <= Floor(AP_Float(n) / 2) - 1 do
    begin
      h[i] := f[i].x - f[i].y;
      h[n - i] := f[i].x + f[i].y;
      inc(i);
    end;
  if n mod 2 = 0 then
    begin
      h[Floor(AP_Float(n) / 2)] := f[Floor(AP_Float(n) / 2)].x;
    end
  else
    begin
      h[Floor(AP_Float(n) / 2)] := f[Floor(AP_Float(n) / 2)].x -
        f[Floor(AP_Float(n) / 2)].y;
      h[Floor(AP_Float(n) / 2) + 1] := f[Floor(AP_Float(n) / 2)].x +
        f[Floor(AP_Float(n) / 2)].y;
    end;
  FFTR1D(h, n, FH);
  i := 0;
  while i <= n - 1 do
    begin
      a[i] := (FH[i].x - FH[i].y) / n;
      inc(i);
    end;
end;

(* ************************************************************************
  Internal subroutine. Never call it directly!
  ************************************************************************ *)
procedure FFTR1DInternalEven(var a: TLVec; n: TLInt; var Buf: TLVec; var Plan: TFTPlan);
var
  x: TLFloat;
  y: TLFloat;
  i: TLInt;
  n2: TLInt;
  idx: TLInt;
  Hn: TLComplex;
  HmnC: TLComplex;
  v: TLComplex;
begin
  Assert((n > 0) and (n mod 2 = 0), 'FFTR1DEvenInplace: incorrect N!');

  //
  // Special cases:
  // * N=2
  //
  // After this block we assume that N is strictly greater than 2
  //
  if n = 2 then
    begin
      x := a[0] + a[1];
      y := a[0] - a[1];
      a[0] := x;
      a[1] := y;
      Exit;
    end;

  //
  // even-size real FFT, use reduction to the TLComplex task
  //
  n2 := n div 2;
  APVMove(@Buf[0], 0, n - 1, @a[0], 0, n - 1);
  FTBaseExecutePlan(Buf, 0, n2, Plan);
  a[0] := Buf[0] + Buf[1];
  i := 1;
  while i <= n2 - 1 do
    begin
      idx := 2 * (i mod n2);
      Hn.x := Buf[idx + 0];
      Hn.y := Buf[idx + 1];
      idx := 2 * (n2 - i);
      HmnC.x := Buf[idx + 0];
      HmnC.y := -Buf[idx + 1];
      v.x := -Sin(-2 * pi * i / n);
      v.y := Cos(-2 * pi * i / n);
      v := C_Sub(C_Add(Hn, HmnC), C_Mul(v, C_Sub(Hn, HmnC)));
      a[2 * i + 0] := 0.5 * v.x;
      a[2 * i + 1] := 0.5 * v.y;
      inc(i);
    end;
  a[1] := Buf[0] - Buf[1];
end;

(* ************************************************************************
  Internal subroutine. Never call it directly!
  ************************************************************************ *)
procedure FFTR1DInvInternalEven(var a: TLVec; n: TLInt; var Buf: TLVec; var Plan: TFTPlan);
var
  x: TLFloat;
  y: TLFloat;
  t: TLFloat;
  i: TLInt;
  n2: TLInt;
begin
  Assert((n > 0) and (n mod 2 = 0), 'FFTR1DInvInternalEven: incorrect N!');

  //
  // Special cases:
  // * N=2
  //
  // After this block we assume that N is strictly greater than 2
  //
  if n = 2 then
    begin
      x := 0.5 * (a[0] + a[1]);
      y := 0.5 * (a[0] - a[1]);
      a[0] := x;
      a[1] := y;
      Exit;
    end;

  //
  // inverse real FFT is reduced to the inverse real FHT,
  // which is reduced to the forward real FHT,
  // which is reduced to the forward real FFT.
  //
  // Don't worry, it is really compact and efficient reduction :)
  //
  n2 := n div 2;
  Buf[0] := a[0];
  i := 1;
  while i <= n2 - 1 do
    begin
      x := a[2 * i + 0];
      y := a[2 * i + 1];
      Buf[i] := x - y;
      Buf[n - i] := x + y;
      inc(i);
    end;
  Buf[n2] := a[1];
  FFTR1DInternalEven(Buf, n, a, Plan);
  a[0] := Buf[0] / n;
  t := AP_Float(1) / n;
  i := 1;
  while i <= n2 - 1 do
    begin
      x := Buf[2 * i + 0];
      y := Buf[2 * i + 1];
      a[i] := t * (x - y);
      a[n - i] := t * (x + y);
      inc(i);
    end;
  a[n2] := Buf[1] / n;
end;
