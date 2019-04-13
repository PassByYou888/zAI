{ ****************************************************************************** }
{ * machine Learn base type support, by QQ 600585@qq.com                       * }
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

type
  TLearnCommState = record
    Stage: TLInt;
    IA: TLIVec;
    BA: TLBVec;
    RA: TLVec;
    ca: TLComplexVec;
  end;

  /// //////////////////////////////////////////////////////////////////////
  // Functions/procedures
  /// //////////////////////////////////////////////////////////////////////
function AbsReal(x: TLFloat): TLFloat;
begin
  // Result := Abs(X);
  if x >= 0 then
      AbsReal := x
  else
      AbsReal := -x;
end;

function AbsInt(i: TLInt): TLInt;
begin
  // Result := Abs(I);
  if i >= 0 then
      AbsInt := i
  else
      AbsInt := -i;
end;

function RandomReal(): TLFloat;
begin
  RandomReal := Random;
end;

function RandomInteger(i: TLInt): TLInt;
begin
  RandomInteger := Random(i);
end;

function Sign(x: TLFloat): TLInt;
begin
  if x > 0 then
      Sign := 1
  else if x < 0 then
      Sign := -1
  else
      Sign := 0;
end;

function AP_Sqr(x: TLFloat): TLFloat;
begin
  AP_Sqr := x * x;
end;

/// //////////////////////////////////////////////////////////////////////
// dynamical arrays copying
/// //////////////////////////////////////////////////////////////////////
function DynamicArrayCopy(const a: TLIVec): TLIVec; overload;
var
  i: TLInt;
begin
  SetLength(Result, length(a));
  for i := low(a) to high(a) do
      Result[i] := a[i];
end;

function DynamicArrayCopy(const a: TLVec): TLVec; overload;
var
  i: TLInt;
begin
  SetLength(Result, length(a));
  for i := low(a) to high(a) do
      Result[i] := a[i];
end;

function DynamicArrayCopy(const a: TLComplexVec): TLComplexVec; overload;
var
  i: TLInt;
begin
  SetLength(Result, length(a));
  for i := low(a) to high(a) do
      Result[i] := a[i];
end;

function DynamicArrayCopy(const a: TLBVec): TLBVec; overload;
var
  i: TLInt;
begin
  SetLength(Result, length(a));
  for i := low(a) to high(a) do
      Result[i] := a[i];
end;

function DynamicArrayCopy(const a: TLIMatrix): TLIMatrix; overload;
var
  i, j: TLInt;
begin
  SetLength(Result, length(a));
  for i := low(a) to high(a) do
    begin
      SetLength(Result[i], length(a[i]));
      for j := low(a[i]) to high(a[i]) do
          Result[i, j] := a[i, j];
    end;
end;

function DynamicArrayCopy(const a: TLMatrix): TLMatrix; overload;
var
  i, j: TLInt;
begin
  SetLength(Result, length(a));
  for i := low(a) to high(a) do
    begin
      SetLength(Result[i], length(a[i]));
      for j := low(a[i]) to high(a[i]) do
          Result[i, j] := a[i, j];
    end;
end;

function DynamicArrayCopy(const a: TLComplexMatrix): TLComplexMatrix; overload;
var
  i, j: TLInt;
begin
  SetLength(Result, length(a));
  for i := low(a) to high(a) do
    begin
      SetLength(Result[i], length(a[i]));
      for j := low(a[i]) to high(a[i]) do
          Result[i, j] := a[i, j];
    end;
end;

function DynamicArrayCopy(const a: TLBMatrix): TLBMatrix; overload;
var
  i, j: TLInt;
begin
  SetLength(Result, length(a));
  for i := low(a) to high(a) do
    begin
      SetLength(Result[i], length(a[i]));
      for j := low(a[i]) to high(a[i]) do
          Result[i, j] := a[i, j];
    end;
end;

/// //////////////////////////////////////////////////////////////////////
// TLComplex numbers
/// //////////////////////////////////////////////////////////////////////
function AbsComplex(const z: TLComplex): TLFloat;
var
  w: TLFloat;
  XABS: TLFloat;
  YABS: TLFloat;
  v: TLFloat;
begin
  XABS := AbsReal(z.x);
  YABS := AbsReal(z.y);
  w := Max(XABS, YABS);
  v := Min(XABS, YABS);
  if v = 0 then
    begin
      AbsComplex := w;
    end
  else
    begin
      AbsComplex := w * Sqrt(1 + Sqr(v / w));
    end;
end;

function Conj(const z: TLComplex): TLComplex;
begin
  Conj.x := z.x;
  Conj.y := -z.y;
end;

function CSqr(const z: TLComplex): TLComplex;
begin
  CSqr.x := Sqr(z.x) - Sqr(z.y);
  CSqr.y := 2 * z.x * z.y;
end;

function C_Complex(const x: TLFloat): TLComplex;
begin
  C_Complex.x := x;
  C_Complex.y := 0;
end;

function C_Opposite(const z: TLComplex): TLComplex;
begin
  C_Opposite.x := -z.x;
  C_Opposite.y := -z.y;
end;

function C_Add(const z1: TLComplex; const z2: TLComplex): TLComplex;
begin
  C_Add.x := z1.x + z2.x;
  C_Add.y := z1.y + z2.y;
end;

function C_Mul(const z1: TLComplex; const z2: TLComplex): TLComplex;
begin
  C_Mul.x := z1.x * z2.x - z1.y * z2.y;
  C_Mul.y := z1.x * z2.y + z1.y * z2.x;
end;

function C_AddR(const z1: TLComplex; const r: TLFloat): TLComplex;
begin
  C_AddR.x := z1.x + r;
  C_AddR.y := z1.y;
end;

function C_MulR(const z1: TLComplex; const r: TLFloat): TLComplex;
begin
  C_MulR.x := z1.x * r;
  C_MulR.y := z1.y * r;
end;

function C_Sub(const z1: TLComplex; const z2: TLComplex): TLComplex;
begin
  C_Sub.x := z1.x - z2.x;
  C_Sub.y := z1.y - z2.y;
end;

function C_SubR(const z1: TLComplex; const r: TLFloat): TLComplex;
begin
  C_SubR.x := z1.x - r;
  C_SubR.y := z1.y;
end;

function C_RSub(const r: TLFloat; const z1: TLComplex): TLComplex;
begin
  C_RSub.x := r - z1.x;
  C_RSub.y := -z1.y;
end;

function C_Div(const z1: TLComplex; const z2: TLComplex): TLComplex;
var
  a: TLFloat;
  b: TLFloat;
  c: TLFloat;
  d: TLFloat;
  E: TLFloat;
  f: TLFloat;
begin
  a := z1.x;
  b := z1.y;
  c := z2.x;
  d := z2.y;
  if AbsReal(d) < AbsReal(c) then
    begin
      E := d / c;
      f := c + d * E;
      C_Div.x := (a + b * E) / f;
      C_Div.y := (b - a * E) / f;
    end
  else
    begin
      E := c / d;
      f := d + c * E;
      C_Div.x := (b + a * E) / f;
      C_Div.y := (-a + b * E) / f;
    end;
end;

function C_DivR(const z1: TLComplex; const r: TLFloat): TLComplex;
begin
  C_DivR.x := z1.x / r;
  C_DivR.y := z1.y / r;
end;

function C_RDiv(const r: TLFloat; const z2: TLComplex): TLComplex;
var
  a: TLFloat;
  c: TLFloat;
  d: TLFloat;
  E: TLFloat;
  f: TLFloat;
begin
  a := r;
  c := z2.x;
  d := z2.y;
  if AbsReal(d) < AbsReal(c) then
    begin
      E := d / c;
      f := c + d * E;
      C_RDiv.x := a / f;
      C_RDiv.y := -a * E / f;
    end
  else
    begin
      E := c / d;
      f := d + c * E;
      C_RDiv.x := a * E / f;
      C_RDiv.y := -a / f;
    end;
end;

function C_Equal(const z1: TLComplex; const z2: TLComplex): Boolean;
begin
  C_Equal := (z1.x = z2.x) and (z1.y = z2.y);
end;

function C_NotEqual(const z1: TLComplex; const z2: TLComplex): Boolean;
begin
  C_NotEqual := (z1.x <> z2.x) or (z1.y <> z2.y);
end;

function C_EqualR(const z1: TLComplex; const r: TLFloat): Boolean;
begin
  C_EqualR := (z1.x = r) and (z1.y = 0);
end;

function C_NotEqualR(const z1: TLComplex; const r: TLFloat): Boolean;
begin
  C_NotEqualR := (z1.x <> r) or (z1.y <> 0);
end;

function APVDotProduct(v1: PLFloat; i11, i12: TLInt; v2: PLFloat; i21, i22: TLInt): TLFloat;
var
  i, c: TLInt;
  r: TLFloat;
begin
  Assert(i12 - i11 = i22 - i21, 'APVDotProduct: arrays of different size!');
  inc(v1, i11);
  inc(v2, i21);

  //
  // Generic pascal code
  //
  c := i12 - i11;
  r := 0;
  for i := 0 to c do
    begin
      r := r + v1^ * v2^;
      inc(v1);
      inc(v2);
    end;
  APVDotProduct := r;
end;

procedure APVMove(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat;
  i21, i22: TLInt); overload;
var
  i, c: TLInt;
begin
  Assert(i12 - i11 = i22 - i21, 'APVMove: arrays of different size!');
  inc(VDst, i11);
  inc(vSrc, i21);

  //
  // Generic pascal code
  //
  c := i12 - i11;
  for i := 0 to c do
    begin
      VDst^ := vSrc^;
      inc(VDst);
      inc(vSrc);
    end;
end;

procedure APVMove(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat;
  i21, i22: TLInt; s: TLFloat); overload;
var
  i, c: TLInt;
begin
  Assert(i12 - i11 = i22 - i21, 'APVMove: arrays of different size!');
  inc(VDst, i11);
  inc(vSrc, i21);

  //
  // Generic pascal code
  //
  c := i12 - i11;
  for i := 0 to c do
    begin
      VDst^ := s * vSrc^;
      inc(VDst);
      inc(vSrc);
    end;
end;

procedure APVMoveNeg(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat;
  i21, i22: TLInt);
var
  i, c: TLInt;
begin
  Assert(i12 - i11 = i22 - i21, 'APVMoveNeg: arrays of different size!');
  inc(VDst, i11);
  inc(vSrc, i21);

  //
  // Generic pascal code
  //
  c := i12 - i11;
  for i := 0 to c do
    begin
      VDst^ := -vSrc^;
      inc(VDst);
      inc(vSrc);
    end;
end;

procedure APVAdd(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat;
  i21, i22: TLInt); overload;
var
  i, c: TLInt;
begin
  Assert(i12 - i11 = i22 - i21, 'APVAdd: arrays of different size!');
  inc(VDst, i11);
  inc(vSrc, i21);

  //
  // Generic pascal code
  //
  c := i12 - i11;
  for i := 0 to c do
    begin
      VDst^ := VDst^ + vSrc^;
      inc(VDst);
      inc(vSrc);
    end;
end;

procedure APVAdd(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat;
  i21, i22: TLInt; s: TLFloat); overload;
var
  i, c: TLInt;
begin
  Assert(i12 - i11 = i22 - i21, 'APVAdd: arrays of different size!');
  inc(VDst, i11);
  inc(vSrc, i21);

  //
  // Generic pascal code
  //
  c := i12 - i11;
  for i := 0 to c do
    begin
      VDst^ := VDst^ + s * vSrc^;
      inc(VDst);
      inc(vSrc);
    end;
end;

procedure APVSub(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat;
  i21, i22: TLInt); overload;
var
  i, c: TLInt;
begin
  Assert(i12 - i11 = i22 - i21, 'APVSub arrays of different size!');
  inc(VDst, i11);
  inc(vSrc, i21);

  //
  // Generic pascal code
  //
  c := i12 - i11;
  for i := 0 to c do
    begin
      VDst^ := VDst^ - vSrc^;
      inc(VDst);
      inc(vSrc);
    end;
end;

procedure APVSub(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat;
  i21, i22: TLInt; s: TLFloat); overload;
begin
  Assert(i12 - i11 = i22 - i21, 'APVSub: arrays of different size!');
  APVAdd(VDst, i11, i12, vSrc, i21, i22, -s);
end;

procedure APVMul(VOp: PLFloat; i1, i2: TLInt; s: TLFloat);
var
  i, c: TLInt;
begin
  inc(VOp, i1);
  c := i2 - i1;
  for i := 0 to c do
    begin
      VOp^ := s * VOp^;
      inc(VOp);
    end;
end;

procedure APVFillValue(VOp: PLFloat; i1, i2: TLInt; s: TLFloat);
var
  i, c: TLInt;
begin
  inc(VOp, i1);
  c := i2 - i1;
  for i := 0 to c do
    begin
      VOp^ := s;
      inc(VOp);
    end;
end;

/// //////////////////////////////////////////////////////////////////////
// IEEE-compliant functions
/// //////////////////////////////////////////////////////////////////////

function AP_Float(x: TLFloat): TLFloat;
begin
  AP_Float := x;
end;

function AP_FP_Eq(x: TLFloat; y: TLFloat): Boolean;
begin
  AP_FP_Eq := x = y;
end;

function AP_FP_NEq(x: TLFloat; y: TLFloat): Boolean;
begin
  AP_FP_NEq := x <> y;
end;

function AP_FP_Less(x: TLFloat; y: TLFloat): Boolean;
begin
  AP_FP_Less := x < y;
end;

function AP_FP_Less_Eq(x: TLFloat; y: TLFloat): Boolean;
begin
  AP_FP_Less_Eq := x <= y;
end;

function AP_FP_Greater(x: TLFloat; y: TLFloat): Boolean;
begin
  AP_FP_Greater := x > y;
end;

function AP_FP_Greater_Eq(x: TLFloat; y: TLFloat): Boolean;
begin
  AP_FP_Greater_Eq := x >= y;
end;

procedure TagSort(var a: TLVec; const n: TLInt; var p1: TLIVec; var p2: TLIVec);
var
  i: TLInt;
  PV: TLIVec;
  VP: TLIVec;
  LV: TLInt;
  LP: TLInt;
  RV: TLInt;
  RP: TLInt;
begin

  //
  // Special cases
  //
  if n <= 0 then
    begin
      Exit;
    end;
  if n = 1 then
    begin
      SetLength(p1, 0 + 1);
      SetLength(p2, 0 + 1);
      p1[0] := 0;
      p2[0] := 0;
      Exit;
    end;

  //
  // General case, N>1: prepare permutations table P1
  //
  SetLength(p1, n);
  i := 0;
  while i <= n - 1 do
    begin
      p1[i] := i;
      inc(i);
    end;

  //
  // General case, N>1: sort, update P1
  //
  TagSortFastI(a, p1, n);

  //
  // General case, N>1: fill permutations table P2
  //
  // To fill P2 we maintain two arrays:
  // * PV, Position(Value). PV[i] contains position of I-th key at the moment
  // * VP, Value(Position). VP[i] contains key which has position I at the moment
  //
  // At each step we making permutation of two items:
  // Left, which is given by position/value pair LP/LV
  // and Right, which is given by RP/RV
  // and updating PV[] and VP[] correspondingly.
  //
  SetLength(PV, n);
  SetLength(VP, n);
  SetLength(p2, n);
  i := 0;
  while i <= n - 1 do
    begin
      PV[i] := i;
      VP[i] := i;
      inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin

      //
      // calculate LP, LV, RP, RV
      //
      LP := i;
      LV := VP[LP];
      RV := p1[i];
      RP := PV[RV];

      //
      // Fill P2
      //
      p2[i] := RP;

      //
      // update PV and VP
      //
      VP[LP] := RV;
      VP[RP] := LV;
      PV[LV] := RP;
      PV[RV] := LP;
      inc(i);
    end;
end;

procedure TagSortFastI(var a: TLVec; var b: TLIVec; n: TLInt);
var
  i: TLInt;
  k: TLInt;
  t: TLInt;
  tmp: TLFloat;
  TmpI: TLInt;
begin

  //
  // Special cases
  //
  if n <= 1 then
    begin
      Exit;
    end;

  //
  // General case, N>1: sort, update B
  //
  i := 2;
  repeat
    t := i;
    while t <> 1 do
      begin
        k := t div 2;
        if AP_FP_Greater_Eq(a[k - 1], a[t - 1]) then
          begin
            t := 1;
          end
        else
          begin
            tmp := a[k - 1];
            a[k - 1] := a[t - 1];
            a[t - 1] := tmp;
            TmpI := b[k - 1];
            b[k - 1] := b[t - 1];
            b[t - 1] := TmpI;
            t := k;
          end;
      end;
    i := i + 1;
  until not(i <= n);
  i := n - 1;
  repeat
    tmp := a[i];
    a[i] := a[0];
    a[0] := tmp;
    TmpI := b[i];
    b[i] := b[0];
    b[0] := TmpI;
    t := 1;
    while t <> 0 do
      begin
        k := 2 * t;
        if k > i then
          begin
            t := 0;
          end
        else
          begin
            if k < i then
              begin
                if AP_FP_Greater(a[k], a[k - 1]) then
                  begin
                    k := k + 1;
                  end;
              end;
            if AP_FP_Greater_Eq(a[t - 1], a[k - 1]) then
              begin
                t := 0;
              end
            else
              begin
                tmp := a[k - 1];
                a[k - 1] := a[t - 1];
                a[t - 1] := tmp;
                TmpI := b[k - 1];
                b[k - 1] := b[t - 1];
                b[t - 1] := TmpI;
                t := k;
              end;
          end;
      end;
    i := i - 1;
  until not(i >= 1);
end;

procedure TagSortFastR(var a: TLVec; var b: TLVec;
  n: TLInt);
var
  i: TLInt;
  k: TLInt;
  t: TLInt;
  tmp: TLFloat;
  TmpR: TLFloat;
begin

  //
  // Special cases
  //
  if n <= 1 then
    begin
      Exit;
    end;

  //
  // General case, N>1: sort, update B
  //
  i := 2;
  repeat
    t := i;
    while t <> 1 do
      begin
        k := t div 2;
        if AP_FP_Greater_Eq(a[k - 1], a[t - 1]) then
          begin
            t := 1;
          end
        else
          begin
            tmp := a[k - 1];
            a[k - 1] := a[t - 1];
            a[t - 1] := tmp;
            TmpR := b[k - 1];
            b[k - 1] := b[t - 1];
            b[t - 1] := TmpR;
            t := k;
          end;
      end;
    i := i + 1;
  until not(i <= n);
  i := n - 1;
  repeat
    tmp := a[i];
    a[i] := a[0];
    a[0] := tmp;
    TmpR := b[i];
    b[i] := b[0];
    b[0] := TmpR;
    t := 1;
    while t <> 0 do
      begin
        k := 2 * t;
        if k > i then
          begin
            t := 0;
          end
        else
          begin
            if k < i then
              begin
                if AP_FP_Greater(a[k], a[k - 1]) then
                  begin
                    k := k + 1;
                  end;
              end;
            if AP_FP_Greater_Eq(a[t - 1], a[k - 1]) then
              begin
                t := 0;
              end
            else
              begin
                tmp := a[k - 1];
                a[k - 1] := a[t - 1];
                a[t - 1] := tmp;
                TmpR := b[k - 1];
                b[k - 1] := b[t - 1];
                b[t - 1] := TmpR;
                t := k;
              end;
          end;
      end;
    i := i - 1;
  until not(i >= 1);
end;

procedure TagSortFast(var a: TLVec; const n: TLInt);
var
  i: TLInt;
  k: TLInt;
  t: TLInt;
  tmp: TLFloat;
begin

  //
  // Special cases
  //
  if n <= 1 then
    begin
      Exit;
    end;

  //
  // General case, N>1: sort, update B
  //
  i := 2;
  repeat
    t := i;
    while t <> 1 do
      begin
        k := t div 2;
        if AP_FP_Greater_Eq(a[k - 1], a[t - 1]) then
          begin
            t := 1;
          end
        else
          begin
            tmp := a[k - 1];
            a[k - 1] := a[t - 1];
            a[t - 1] := tmp;
            t := k;
          end;
      end;
    i := i + 1;
  until not(i <= n);
  i := n - 1;
  repeat
    tmp := a[i];
    a[i] := a[0];
    a[0] := tmp;
    t := 1;
    while t <> 0 do
      begin
        k := 2 * t;
        if k > i then
          begin
            t := 0;
          end
        else
          begin
            if k < i then
              begin
                if AP_FP_Greater(a[k], a[k - 1]) then
                  begin
                    k := k + 1;
                  end;
              end;
            if AP_FP_Greater_Eq(a[t - 1], a[k - 1]) then
              begin
                t := 0;
              end
            else
              begin
                tmp := a[k - 1];
                a[k - 1] := a[t - 1];
                a[t - 1] := tmp;
                t := k;
              end;
          end;
      end;
    i := i - 1;
  until not(i >= 1);
end;

(* ************************************************************************
  Heap operations: adds element to the heap

  PARAMETERS:
  A       -   heap itself, must be at least array[0..N]
  B       -   array of integer tags, which are updated according to
  permutations in the heap
  N       -   size of the heap (without new element).
  updated on output
  VA      -   value of the element being added
  VB      -   value of the tag
  ************************************************************************ *)
procedure TagHeapPushI(var a: TLVec; var b: TLIVec;
  var n: TLInt; const VA: TLFloat; const VB: TLInt);
var
  j: TLInt;
  k: TLInt;
  v: TLFloat;
begin
  if n < 0 then
    begin
      Exit;
    end;

  //
  // N=0 is a special case
  //
  if n = 0 then
    begin
      a[0] := VA;
      b[0] := VB;
      n := n + 1;
      Exit;
    end;

  //
  // add current point to the heap
  // (add to the bottom, then move up)
  //
  // we don't write point to the heap
  // until its final position is determined
  // (it allow us to reduce number of array access operations)
  //
  j := n;
  n := n + 1;
  while j > 0 do
    begin
      k := (j - 1) div 2;
      v := a[k];
      if AP_FP_Less(v, VA) then
        begin

          //
          // swap with higher element
          //
          a[j] := v;
          b[j] := b[k];
          j := k;
        end
      else
        begin

          //
          // element in its place. terminate.
          //
          Break;
        end;
    end;
  a[j] := VA;
  b[j] := VB;
end;

(* ************************************************************************
  Heap operations: replaces top element with new element
  (which is moved down)

  PARAMETERS:
  A       -   heap itself, must be at least array[0..N-1]
  B       -   array of integer tags, which are updated according to
  permutations in the heap
  N       -   size of the heap
  VA      -   value of the element which replaces top element
  VB      -   value of the tag
  ************************************************************************ *)
procedure TagHeapReplaceTopI(var a: TLVec; var b: TLIVec;
  const n: TLInt; const VA: TLFloat; const VB: TLInt);
var
  j: TLInt;
  k1: TLInt;
  k2: TLInt;
  v: TLFloat;
  v1: TLFloat;
  v2: TLFloat;
begin
  if n < 1 then
    begin
      Exit;
    end;

  //
  // N=1 is a special case
  //
  if n = 1 then
    begin
      a[0] := VA;
      b[0] := VB;
      Exit;
    end;

  //
  // move down through heap:
  // * J  -   current element
  // * K1 -   first child (always exists)
  // * K2 -   second child (may not exists)
  //
  // we don't write point to the heap
  // until its final position is determined
  // (it allow us to reduce number of array access operations)
  //
  j := 0;
  k1 := 1;
  k2 := 2;
  while k1 < n do
    begin
      if k2 >= n then
        begin

          //
          // only one child.
          //
          // swap and terminate (because this child
          // have no siblings due to heap structure)
          //
          v := a[k1];
          if AP_FP_Greater(v, VA) then
            begin
              a[j] := v;
              b[j] := b[k1];
              j := k1;
            end;
          Break;
        end
      else
        begin

          //
          // two childs
          //
          v1 := a[k1];
          v2 := a[k2];
          if AP_FP_Greater(v1, v2) then
            begin
              if AP_FP_Less(VA, v1) then
                begin
                  a[j] := v1;
                  b[j] := b[k1];
                  j := k1;
                end
              else
                begin
                  Break;
                end;
            end
          else
            begin
              if AP_FP_Less(VA, v2) then
                begin
                  a[j] := v2;
                  b[j] := b[k2];
                  j := k2;
                end
              else
                begin
                  Break;
                end;
            end;
          k1 := 2 * j + 1;
          k2 := 2 * j + 2;
        end;
    end;
  a[j] := VA;
  b[j] := VB;
end;

(* ************************************************************************
  Heap operations: pops top element from the heap

  PARAMETERS:
  A       -   heap itself, must be at least array[0..N-1]
  B       -   array of integer tags, which are updated according to
  permutations in the heap
  N       -   size of the heap, N>=1

  On output top element is moved to A[N-1], B[N-1], heap is reordered, N is
  decreased by 1.
  ************************************************************************ *)
procedure TagHeapPopI(var a: TLVec; var b: TLIVec; var n: TLInt);
var
  VA: TLFloat;
  VB: TLInt;
begin
  if n < 1 then
    begin
      Exit;
    end;

  //
  // N=1 is a special case
  //
  if n = 1 then
    begin
      n := 0;
      Exit;
    end;

  //
  // swap top element and last element,
  // then reorder heap
  //
  VA := a[n - 1];
  VB := b[n - 1];
  a[n - 1] := a[0];
  b[n - 1] := b[0];
  n := n - 1;
  TagHeapReplaceTopI(a, b, n, VA, VB);
end;
