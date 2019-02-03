{ ****************************************************************************** }
{ * k-means++ clusterization library  written by QQ 600585@qq.com              * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

unit KM;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses;

type
  TKMInt = Integer;
  PKMInt = ^TKMInt;

  TDynamicIndexArray = array of TKMInt;
  PDynamicIndexArray = ^TDynamicIndexArray;

  TKMBoolArray = array of Boolean;
  PKMBoolArray = ^TKMBoolArray;

  TKMFloat = Double;
  PKMFloat = ^TKMFloat;

  TKMIntegerArray = TDynamicIndexArray;
  PKMIntegerArray = PDynamicIndexArray;

  TKMFloatArray = array of TKMFloat;
  PKMFloatArray = ^TKMFloatArray;

  TKMFloat2DArray = array of TKMFloatArray;
  PKMFloat2DArray = ^TKMFloat2DArray;

  (*
    k-means++ clusterization
    return state:
    * -3, if task is degenerate (number of distinct points is less than K)
    * -1, if incorrect NPoints/NFeatures/K/Restarts was passed
    *  1, if subroutine finished successfully
  *)
function KMeansCluster(const Source: TKMFloat2DArray; const NVars, k, Restarts: NativeInt; var KArray: TKMFloat2DArray; var kIndex: TKMIntegerArray): ShortInt;

implementation

uses Math, Learn;

procedure ArrayMove(VDst: PKMFloat; const i11, i12: NativeInt; vSrc: PKMFloat; const i21, i22: NativeInt); inline; overload;
var
  i: NativeInt;
begin
  inc(VDst, i11);
  inc(vSrc, i21);

  for i := i12 - i11 downto 0 do
    begin
      VDst^ := vSrc^;
      inc(VDst);
      inc(vSrc);
    end;
end;

procedure ArrayMove(VDst: PKMFloat; const i11, i12: NativeInt; vSrc: PKMFloat; const i21, i22: NativeInt; const f: TKMFloat); inline; overload;
var
  i: NativeInt;
begin
  inc(VDst, i11);
  inc(vSrc, i21);

  for i := i12 - i11 downto 0 do
    begin
      VDst^ := f * vSrc^;
      inc(VDst);
      inc(vSrc);
    end;
end;

procedure ArraySub(VDst: PKMFloat; const i11, i12: NativeInt; vSrc: PKMFloat; const i21, i22: NativeInt); inline;
var
  i: NativeInt;
begin
  inc(VDst, i11);
  inc(vSrc, i21);

  for i := i12 - i11 downto 0 do
    begin
      VDst^ := VDst^ - vSrc^;
      inc(VDst);
      inc(vSrc);
    end;
end;

function ArrayDotProduct(v1: PKMFloat; const i11, i12: NativeInt; v2: PKMFloat; const i21, i22: NativeInt): TKMFloat; inline;
var
  i: NativeInt;
begin
  inc(v1, i11);
  inc(v2, i21);

  Result := 0;
  for i := i12 - i11 downto 0 do
    begin
      Result := Result + (v1^ * v2^);
      inc(v1);
      inc(v2);
    end;
end;

procedure ArrayAdd(VDst: PKMFloat; const i11, i12: NativeInt; vSrc: PKMFloat; const i21, i22: NativeInt); inline;
var
  i: NativeInt;
begin
  inc(VDst, i11);
  inc(vSrc, i21);

  for i := i12 - i11 downto 0 do
    begin
      VDst^ := VDst^ + vSrc^;
      inc(VDst);
      inc(vSrc);
    end;
end;

procedure ArrayMul(VOp: PKMFloat; const i1, i2: NativeInt; const f: TKMFloat); inline;
var
  i: NativeInt;
begin
  inc(VOp, i1);
  for i := i2 - i1 downto 0 do
    begin
      VOp^ := f * VOp^;
      inc(VOp);
    end;
end;

procedure CopyMatrix(const a: TKMFloat2DArray; const IS1, IS2, JS1, JS2: NativeInt; var b: TKMFloat2DArray; const ID1, id2, JD1, JD2: NativeInt); inline;
var
  isrc, idst: NativeInt;
begin
  if (IS1 > IS2) or (JS1 > JS2) then
      Exit;
  isrc := IS1;
  while isrc <= IS2 do
    begin
      idst := isrc - IS1 + ID1;
      ArrayMove(@b[idst][0], JD1, JD2, @a[isrc][0], JS1, JS2);
      inc(isrc);
    end;
end;

procedure CopyAndTranspose(const a: TKMFloat2DArray; const IS1, IS2, JS1, JS2: NativeInt; var b: TKMFloat2DArray; const ID1, id2, JD1, JD2: NativeInt); inline;
var
  isrc, jdst, i, k: NativeInt;
begin
  if (IS1 > IS2) or (JS1 > JS2) then
      Exit;
  isrc := IS1;
  while isrc <= IS2 do
    begin
      jdst := isrc - IS1 + JD1;
      k := JS1 - ID1;
      for i := ID1 to id2 do
          b[i, jdst] := a[isrc, i + k];
      inc(isrc);
    end;
end;

procedure DynamicArrayCopy(const Source: TKMBoolArray; var output: TKMBoolArray); inline;
var
  i: NativeInt;
  r: TKMBoolArray;
begin
  SetLength(output, length(Source));
  for i := low(Source) to high(Source) do
      output[i] := Source[i];
end;

(* ************************************************************************
  Select center for a new cluster using k-means++ rule
  ************************************************************************ *)
function SelectCenter(const Source: TKMFloat2DArray;
  const NPoints, NVars: NativeInt; var Centers: TKMFloat2DArray; const BusyCenters: TKMBoolArray; const CCnt: NativeInt;
  var d2: TKMFloatArray; var p: TKMFloatArray; var tmp: TKMFloatArray): Boolean; inline;
var
  NewBusyCenters: TKMBoolArray;
  i: NativeInt;
  j: NativeInt;
  CC: NativeInt;
  v: TKMFloat;
  s: TKMFloat;
begin
  DynamicArrayCopy(BusyCenters, NewBusyCenters);

  Result := True;
  CC := 0;
  while CC <= CCnt - 1 do
    begin
      if not NewBusyCenters[CC] then
        begin
          i := 0;
          while i <= NPoints - 1 do
            begin
              d2[i] := MaxRealNumber;
              j := 0;
              while j <= CCnt - 1 do
                begin
                  if NewBusyCenters[j] then
                    begin
                      ArrayMove(@tmp[0], 0, NVars - 1, @Source[i][0], 0, NVars - 1);
                      ArraySub(@tmp[0], 0, NVars - 1, @Centers[j][0], 0, NVars - 1);
                      v := ArrayDotProduct(@tmp[0], 0, NVars - 1, @tmp[0], 0, NVars - 1);
                      if v < d2[i] then
                          d2[i] := v;
                    end;
                  inc(j);
                end;
              inc(i);
            end;

          // calculate P (non-cumulative)
          s := 0;
          i := 0;
          while i <= NPoints - 1 do
            begin
              s := s + d2[i];
              inc(i);
            end;
          if s = 0 then
            begin
              Result := False;
              Exit;
            end;
          s := 1 / s;
          ArrayMove(@p[0], 0, NPoints - 1, @d2[0], 0, NPoints - 1, s);

          // choose one of points with probability P
          // random number within (0,1) is generated and
          // inverse empirical CDF is used to randomly choose a point.
          s := 0;
          v := RandomReal();
          i := 0;
          while i <= NPoints - 1 do
            begin
              s := s + p[i];
              if (v <= s) or (i = NPoints - 1) then
                begin
                  ArrayMove(@Centers[CC][0], 0, NVars - 1, @Source[i][0], 0, NVars - 1);
                  NewBusyCenters[CC] := True;
                  Break;
                end;
              inc(i);
            end;
        end;
      inc(CC);
    end;
end;

function KMeansCluster(const Source: TKMFloat2DArray;
  const NVars, k, Restarts: NativeInt; var KArray: TKMFloat2DArray; var kIndex: TKMIntegerArray): ShortInt;
var
  NPoints: NativeInt;
  i: NativeInt;
  j: NativeInt;
  ct: TKMFloat2DArray;
  CTBest: TKMFloat2DArray;
  XYCBest: TKMIntegerArray;
  E: TKMFloat;
  EBest: TKMFloat;
  x: TKMFloatArray;
  tmp: TKMFloatArray;
  d2: TKMFloatArray;
  p: TKMFloatArray;
  CSizes: TKMIntegerArray;
  CBusy: TKMBoolArray;
  v: TKMFloat;
  CClosest: NativeInt;
  DClosest: TKMFloat;
  Work: TKMFloatArray;
  WasChanges: Boolean;
  ZeroSizeClusters: Boolean;
  pass: NativeInt;
begin
  NPoints := length(Source);

  if (NPoints < k) or (NVars < 1) or (k < 1) or (Restarts < 1) then
    begin
      Result := -1;
      Exit;
    end;

  Result := 1;

  SetLength(ct, k, NVars);
  SetLength(CTBest, k, NVars);
  SetLength(kIndex, NPoints);
  SetLength(XYCBest, NPoints);
  SetLength(d2, NPoints);
  SetLength(p, NPoints);
  SetLength(tmp, NVars);
  SetLength(CSizes, k);
  SetLength(CBusy, k);
  EBest := MaxRealNumber;
  pass := 1;
  while pass <= Restarts do
    begin
      // Select initial centers  using k-means++ algorithm
      // 1. Choose first center at random
      // 2. Choose next centers using their distance from centers already chosen
      //
      // Note that for performance reasons centers are stored in ROWS of CT, not
      // in columns. We'll transpose CT in the end and store it in the KArray.
      i := RandomInteger(NPoints);
      ArrayMove(@ct[0][0], 0, NVars - 1, @Source[i][0], 0, NVars - 1);
      CBusy[0] := True;
      i := 1;
      while i <= k - 1 do
        begin
          CBusy[i] := False;
          inc(i);
        end;
      if not SelectCenter(Source, NPoints, NVars, ct, CBusy, k, d2, p, tmp) then
        begin
          Result := -3;
          Exit;
        end;

      // Update centers:
      // 2. update center positions
      while True do
        begin
          // fill kIndex with center numbers
          WasChanges := False;
          i := 0;
          while i <= NPoints - 1 do
            begin
              CClosest := -1;
              DClosest := MaxRealNumber;
              j := 0;
              while j <= k - 1 do
                begin
                  ArrayMove(@tmp[0], 0, NVars - 1, @Source[i][0], 0, NVars - 1);
                  ArraySub(@tmp[0], 0, NVars - 1, @ct[j][0], 0, NVars - 1);
                  v := ArrayDotProduct(@tmp[0], 0, NVars - 1, @tmp[0], 0, NVars - 1);
                  if v < DClosest then
                    begin
                      CClosest := j;
                      DClosest := v;
                    end;
                  inc(j);
                end;
              if kIndex[i] <> CClosest then
                  WasChanges := True;
              kIndex[i] := CClosest;
              inc(i);
            end;

          // Update centers
          j := 0;
          while j <= k - 1 do
            begin
              CSizes[j] := 0;
              inc(j);
            end;
          i := 0;
          while i <= k - 1 do
            begin
              j := 0;
              while j <= NVars - 1 do
                begin
                  ct[i, j] := 0;
                  inc(j);
                end;
              inc(i);
            end;
          i := 0;
          while i <= NPoints - 1 do
            begin
              CSizes[kIndex[i]] := CSizes[kIndex[i]] + 1;
              ArrayAdd(@ct[kIndex[i]][0], 0, NVars - 1, @Source[i][0], 0, NVars - 1);
              inc(i);
            end;
          ZeroSizeClusters := False;
          i := 0;
          while i <= k - 1 do
            begin
              CBusy[i] := CSizes[i] <> 0;
              ZeroSizeClusters := ZeroSizeClusters or (CSizes[i] = 0);
              inc(i);
            end;
          if ZeroSizeClusters then
            begin
              // Some clusters have zero size - rare, but possible.
              // We'll choose new centers for such clusters using k-means++ rule
              // and restart algorithm
              if not SelectCenter(Source, NPoints, NVars, ct, CBusy, k, d2, p, tmp) then
                begin
                  Result := -3;
                  Exit;
                end;
              Continue;
            end;
          j := 0;
          while j <= k - 1 do
            begin
              v := 1.0 / CSizes[j];
              ArrayMul(@ct[j][0], 0, NVars - 1, v);
              inc(j);
            end;

          // if nothing has changed during iteration
          if not WasChanges then
              Break;
        end;

      // 3. Calculate E, compare with best centers found so far
      E := 0;
      i := 0;
      while i <= NPoints - 1 do
        begin
          ArrayMove(@tmp[0], 0, NVars - 1, @Source[i][0], 0, NVars - 1);
          ArraySub(@tmp[0], 0, NVars - 1, @ct[kIndex[i]][0], 0, NVars - 1);
          v := ArrayDotProduct(@tmp[0], 0, NVars - 1, @tmp[0], 0, NVars - 1);
          E := E + v;
          inc(i);
        end;
      if E < EBest then
        begin
          // store partition.
          EBest := E;
          CopyMatrix(ct, 0, k - 1, 0, NVars - 1, CTBest, 0, k - 1, 0, NVars - 1);
          i := 0;
          while i <= NPoints - 1 do
            begin
              XYCBest[i] := kIndex[i];
              inc(i);
            end;
        end;
      inc(pass);
    end;

  // Copy and transpose
  SetLength(KArray, NVars - 1 + 1, k - 1 + 1);
  CopyAndTranspose(CTBest, 0, k - 1, 0, NVars - 1, KArray, 0, NVars - 1, 0, k - 1);
  i := 0;
  while i <= NPoints - 1 do
    begin
      kIndex[i] := XYCBest[i];
      inc(i);
    end;
end;

initialization

finalization

end.
