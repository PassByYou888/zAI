{ ****************************************************************************** }
{ * test support, by QQ 600585@qq.com                                          * }
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

(* ************************************************************************
  This  function  generates  1-dimensional  general  interpolation task with
  moderate Lipshitz constant (close to 1.0)

  If N=1 then suborutine generates only one point at the middle of [A,B]
  ************************************************************************ *)
procedure TaskGenInt1D(a: TLFloat; b: TLFloat; n: TLInt; var x: TLVec; var y: TLVec);
var
  i: TLInt;
  h: TLFloat;
begin
  Assert(n >= 1, 'TaskGenInterpolationEqdist1D: N<1!');
  SetLength(x, n);
  SetLength(y, n);
  if n > 1 then
    begin
      x[0] := a;
      y[0] := 2 * RandomReal - 1;
      h := (b - a) / (n - 1);
      i := 1;
      while i <= n - 1 do
        begin
          if i <> n - 1 then
            begin
              x[i] := a + (i + 0.2 * (2 * RandomReal - 1)) * h;
            end
          else
            begin
              x[i] := b;
            end;
          y[i] := y[i - 1] + (2 * RandomReal - 1) * (x[i] - x[i - 1]);
          inc(i);
        end;
    end
  else
    begin
      x[0] := 0.5 * (a + b);
      y[0] := 2 * RandomReal - 1;
    end;
end;

(* ************************************************************************
  This function generates  1-dimensional equidistant interpolation task with
  moderate Lipshitz constant (close to 1.0)

  If N=1 then suborutine generates only one point at the middle of [A,B]
  ************************************************************************ *)
procedure TaskGenInt1DEquidist(a: TLFloat; b: TLFloat; n: TLInt; var x: TLVec; var y: TLVec);
var
  i: TLInt;
  h: TLFloat;
begin
  Assert(n >= 1, 'TaskGenInterpolationEqdist1D: N<1!');
  SetLength(x, n);
  SetLength(y, n);
  if n > 1 then
    begin
      x[0] := a;
      y[0] := 2 * RandomReal - 1;
      h := (b - a) / (n - 1);
      i := 1;
      while i <= n - 1 do
        begin
          x[i] := a + i * h;
          y[i] := y[i - 1] + (2 * RandomReal - 1) * h;
          inc(i);
        end;
    end
  else
    begin
      x[0] := 0.5 * (a + b);
      y[0] := 2 * RandomReal - 1;
    end;
end;

(* ************************************************************************
  This function generates  1-dimensional Chebyshev-1 interpolation task with
  moderate Lipshitz constant (close to 1.0)

  If N=1 then suborutine generates only one point at the middle of [A,B]
  ************************************************************************ *)
procedure TaskGenInt1DCheb1(a: TLFloat; b: TLFloat; n: TLInt; var x: TLVec; var y: TLVec);
var
  i: TLInt;
begin
  Assert(n >= 1, 'TaskGenInterpolation1DCheb1: N<1!');
  SetLength(x, n);
  SetLength(y, n);
  if n > 1 then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          x[i] := 0.5 * (b + a) + 0.5 * (b - a) * Cos(pi * (2 * i + 1) / (2 * n));
          if i = 0 then
            begin
              y[i] := 2 * RandomReal - 1;
            end
          else
            begin
              y[i] := y[i - 1] + (2 * RandomReal - 1) * (x[i] - x[i - 1]);
            end;
          inc(i);
        end;
    end
  else
    begin
      x[0] := 0.5 * (a + b);
      y[0] := 2 * RandomReal - 1;
    end;
end;

(* ************************************************************************
  This function generates  1-dimensional Chebyshev-2 interpolation task with
  moderate Lipshitz constant (close to 1.0)

  If N=1 then suborutine generates only one point at the middle of [A,B]
  ************************************************************************ *)
procedure TaskGenInt1DCheb2(a: TLFloat; b: TLFloat; n: TLInt; var x: TLVec; var y: TLVec);
var
  i: TLInt;
begin
  Assert(n >= 1, 'TaskGenInterpolation1DCheb2: N<1!');
  SetLength(x, n);
  SetLength(y, n);
  if n > 1 then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          x[i] := 0.5 * (b + a) + 0.5 * (b - a) * Cos(pi * i / (n - 1));
          if i = 0 then
            begin
              y[i] := 2 * RandomReal - 1;
            end
          else
            begin
              y[i] := y[i - 1] + (2 * RandomReal - 1) * (x[i] - x[i - 1]);
            end;
          inc(i);
        end;
    end
  else
    begin
      x[0] := 0.5 * (a + b);
      y[0] := 2 * RandomReal - 1;
    end;
end;

(* ************************************************************************
  This function checks that all values from X[] are distinct. It does more
  than just usual floating point comparison:
  * first, it calculates max(X) and min(X)
  * second, it maps X[] from [min,max] to [1,2]
  * only at this stage actual comparison is done

  The meaning of such check is to ensure that all values are "distinct enough"
  and will not cause interpolation subroutine to fail.

  NOTE:
  X[] must be sorted by ascending (subroutine ASSERT's it)
  ************************************************************************ *)
function APSERVAreDistinct(x: TLVec; n: TLInt): Boolean;
var
  IsSorted: Boolean;
  a: TLFloat;
  b: TLFloat;
  i: TLInt;
begin
  x := DynamicArrayCopy(x);
  Assert(n >= 1, 'APSERVIsDistinct: internal error!');
  if n = 1 then
    begin

      //
      // everything is alright, it is up to caller to decide whether it
      // can interpolate something with just one point
      //
      Result := True;
      Exit;
    end;
  a := x[0];
  b := x[0];
  i := 1;
  while i <= n - 1 do
    begin
      a := Min(a, x[i]);
      b := Max(b, x[i]);
      Assert(AP_FP_Greater_Eq(x[i], x[i - 1]),
        'APSERVIsDistinct: internal error!');
      inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      x[i] := (x[i] - a) / (b - a) + 1;
      inc(i);
    end;
  i := 1;
  while i <= n - 1 do
    begin
      if AP_FP_Eq(x[i], x[i - 1]) then
        begin
          Result := False;
          Exit;
        end;
      inc(i);
    end;
  Result := True;
end;

(* ************************************************************************
  Safe sqrt(x^2+y^2)
  ************************************************************************ *)
function SafePythag2(x: TLFloat; y: TLFloat): TLFloat;
var
  w: TLFloat;
  XABS: TLFloat;
  YABS: TLFloat;
  z: TLFloat;
begin
  XABS := AbsReal(x);
  YABS := AbsReal(y);
  w := Max(XABS, YABS);
  z := Min(XABS, YABS);
  if AP_FP_Eq(z, 0) then
    begin
      Result := w;
    end
  else
    begin
      Result := w * Sqrt(1 + AP_Sqr(z / w));
    end;
end;

(* ************************************************************************
  Safe sqrt(x^2+y^2)
  ************************************************************************ *)
function SafePythag3(x: TLFloat; y: TLFloat; z: TLFloat): TLFloat;
var
  w: TLFloat;
begin
  w := Max(AbsReal(x), Max(AbsReal(y), AbsReal(z)));
  if AP_FP_Eq(w, 0) then
    begin
      Result := 0;
      Exit;
    end;
  x := x / w;
  y := y / w;
  z := z / w;
  Result := w * Sqrt(AP_Sqr(x) + AP_Sqr(y) + AP_Sqr(z));
end;

function TestABLAS(Silent: Boolean): Boolean;
  function InternalCMatrixTRInverse(var a: TLComplexMatrix; n: TLInt; IsUpper: Boolean; IsunitTriangular: Boolean): Boolean;
  var
    NOunit: Boolean;
    i: TLInt;
    j: TLInt;
    v: TLComplex;
    AJJ: TLComplex;
    t: TLComplexVec;
    i_: TLInt;
  begin
    Result := True;
    SetLength(t, n);

    //
    // Test the input parameters.
    //
    NOunit := not IsunitTriangular;
    if IsUpper then
      begin

        //
        // Compute inverse of upper triangular matrix.
        //
        j := 0;
        while j <= n - 1 do
          begin
            if NOunit then
              begin
                if C_EqualR(a[j, j], 0) then
                  begin
                    Result := False;
                    Exit;
                  end;
                a[j, j] := C_RDiv(1, a[j, j]);
                AJJ := C_Opposite(a[j, j]);
              end
            else
              begin
                AJJ := C_Complex(-1);
              end;

            //
            // Compute elements 1:j-1 of j-th column.
            //
            if j > 0 then
              begin
                for i_ := 0 to j - 1 do
                  begin
                    t[i_] := a[i_, j];
                  end;
                i := 0;
                while i <= j - 1 do
                  begin
                    if i + 1 < j then
                      begin
                        v := C_Complex(0.0);
                        for i_ := i + 1 to j - 1 do
                          begin
                            v := C_Add(v, C_Mul(a[i, i_], t[i_]));
                          end;
                      end
                    else
                      begin
                        v := C_Complex(0);
                      end;
                    if NOunit then
                      begin
                        a[i, j] := C_Add(v, C_Mul(a[i, i], t[i]));
                      end
                    else
                      begin
                        a[i, j] := C_Add(v, t[i]);
                      end;
                    inc(i);
                  end;
                for i_ := 0 to j - 1 do
                  begin
                    a[i_, j] := C_Mul(AJJ, a[i_, j]);
                  end;
              end;
            inc(j);
          end;
      end
    else
      begin

        //
        // Compute inverse of lower triangular matrix.
        //
        j := n - 1;
        while j >= 0 do
          begin
            if NOunit then
              begin
                if C_EqualR(a[j, j], 0) then
                  begin
                    Result := False;
                    Exit;
                  end;
                a[j, j] := C_RDiv(1, a[j, j]);
                AJJ := C_Opposite(a[j, j]);
              end
            else
              begin
                AJJ := C_Complex(-1);
              end;
            if j + 1 < n then
              begin

                //
                // Compute elements j+1:n of j-th column.
                //
                for i_ := j + 1 to n - 1 do
                  begin
                    t[i_] := a[i_, j];
                  end;
                i := j + 1;
                while i <= n - 1 do
                  begin
                    if i > j + 1 then
                      begin
                        v := C_Complex(0.0);
                        for i_ := j + 1 to i - 1 do
                          begin
                            v := C_Add(v, C_Mul(a[i, i_], t[i_]));
                          end;
                      end
                    else
                      begin
                        v := C_Complex(0);
                      end;
                    if NOunit then
                      begin
                        a[i, j] := C_Add(v, C_Mul(a[i, i], t[i]));
                      end
                    else
                      begin
                        a[i, j] := C_Add(v, t[i]);
                      end;
                    inc(i);
                  end;
                for i_ := j + 1 to n - 1 do
                  begin
                    a[i_, j] := C_Mul(AJJ, a[i_, j]);
                  end;
              end;
            dec(j);
          end;
      end;
  end;

  function InternalRMatrixTRInverse(var a: TLMatrix; n: TLInt; IsUpper: Boolean; IsunitTriangular: Boolean): Boolean;
  var
    NOunit: Boolean;
    i: TLInt;
    j: TLInt;
    v: TLFloat;
    AJJ: TLFloat;
    t: TLVec;
    i_: TLInt;
  begin
    Result := True;
    SetLength(t, n);

    //
    // Test the input parameters.
    //
    NOunit := not IsunitTriangular;
    if IsUpper then
      begin

        //
        // Compute inverse of upper triangular matrix.
        //
        j := 0;
        while j <= n - 1 do
          begin
            if NOunit then
              begin
                if AP_FP_Eq(a[j, j], 0) then
                  begin
                    Result := False;
                    Exit;
                  end;
                a[j, j] := 1 / a[j, j];
                AJJ := -a[j, j];
              end
            else
              begin
                AJJ := -1;
              end;

            //
            // Compute elements 1:j-1 of j-th column.
            //
            if j > 0 then
              begin
                for i_ := 0 to j - 1 do
                  begin
                    t[i_] := a[i_, j];
                  end;
                i := 0;
                while i <= j - 1 do
                  begin
                    if i < j - 1 then
                      begin
                        v := APVDotProduct(@a[i][0], i + 1, j - 1, @t[0], i + 1, j - 1);
                      end
                    else
                      begin
                        v := 0;
                      end;
                    if NOunit then
                      begin
                        a[i, j] := v + a[i, i] * t[i];
                      end
                    else
                      begin
                        a[i, j] := v + t[i];
                      end;
                    inc(i);
                  end;
                for i_ := 0 to j - 1 do
                  begin
                    a[i_, j] := AJJ * a[i_, j];
                  end;
              end;
            inc(j);
          end;
      end
    else
      begin

        //
        // Compute inverse of lower triangular matrix.
        //
        j := n - 1;
        while j >= 0 do
          begin
            if NOunit then
              begin
                if AP_FP_Eq(a[j, j], 0) then
                  begin
                    Result := False;
                    Exit;
                  end;
                a[j, j] := 1 / a[j, j];
                AJJ := -a[j, j];
              end
            else
              begin
                AJJ := -1;
              end;
            if j < n - 1 then
              begin

                //
                // Compute elements j+1:n of j-th column.
                //
                for i_ := j + 1 to n - 1 do
                  begin
                    t[i_] := a[i_, j];
                  end;
                i := j + 1;
                while i <= n - 1 do
                  begin
                    if i > j + 1 then
                      begin
                        v := APVDotProduct(@a[i][0], j + 1, i - 1, @t[0], j + 1, i - 1);
                      end
                    else
                      begin
                        v := 0;
                      end;
                    if NOunit then
                      begin
                        a[i, j] := v + a[i, i] * t[i];
                      end
                    else
                      begin
                        a[i, j] := v + t[i];
                      end;
                    inc(i);
                  end;
                for i_ := j + 1 to n - 1 do
                  begin
                    a[i_, j] := AJJ * a[i_, j];
                  end;
              end;
            dec(j);
          end;
      end;
  end;

  procedure RefCMatrixRightTRSM(M: TLInt; n: TLInt; const a: TLComplexMatrix; i1: TLInt; j1: TLInt; IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt; var x: TLComplexMatrix; i2: TLInt; j2: TLInt);
  var
    a1: TLComplexMatrix;
    a2: TLComplexMatrix;
    TX: TLComplexVec;
    i: TLInt;
    j: TLInt;
    VC: TLComplex;
    RUpper: Boolean;
    i_: TLInt;
    i1_: TLInt;
  begin
    if n * M = 0 then
      begin
        Exit;
      end;
    SetLength(a1, n, n);
    i := 0;
    while i <= n - 1 do
      begin
        j := 0;
        while j <= n - 1 do
          begin
            a1[i, j] := C_Complex(0);
            inc(j);
          end;
        inc(i);
      end;
    if IsUpper then
      begin
        i := 0;
        while i <= n - 1 do
          begin
            j := i;
            while j <= n - 1 do
              begin
                a1[i, j] := a[i1 + i, j1 + j];
                inc(j);
              end;
            inc(i);
          end;
      end
    else
      begin
        i := 0;
        while i <= n - 1 do
          begin
            j := 0;
            while j <= i do
              begin
                a1[i, j] := a[i1 + i, j1 + j];
                inc(j);
              end;
            inc(i);
          end;
      end;
    RUpper := IsUpper;
    if IsUnit then
      begin
        i := 0;
        while i <= n - 1 do
          begin
            a1[i, i] := C_Complex(1);
            inc(i);
          end;
      end;
    SetLength(a2, n, n);
    if OpType = 0 then
      begin
        i := 0;
        while i <= n - 1 do
          begin
            j := 0;
            while j <= n - 1 do
              begin
                a2[i, j] := a1[i, j];
                inc(j);
              end;
            inc(i);
          end;
      end;
    if OpType = 1 then
      begin
        i := 0;
        while i <= n - 1 do
          begin
            j := 0;
            while j <= n - 1 do
              begin
                a2[i, j] := a1[j, i];
                inc(j);
              end;
            inc(i);
          end;
        RUpper := not RUpper;
      end;
    if OpType = 2 then
      begin
        i := 0;
        while i <= n - 1 do
          begin
            j := 0;
            while j <= n - 1 do
              begin
                a2[i, j] := Conj(a1[j, i]);
                inc(j);
              end;
            inc(i);
          end;
        RUpper := not RUpper;
      end;
    InternalCMatrixTRInverse(a2, n, RUpper, False);
    SetLength(TX, n);
    i := 0;
    while i <= M - 1 do
      begin
        i1_ := (j2) - (0);
        for i_ := 0 to n - 1 do
          begin
            TX[i_] := x[i2 + i, i_ + i1_];
          end;
        j := 0;
        while j <= n - 1 do
          begin
            VC := C_Complex(0.0);
            for i_ := 0 to n - 1 do
              begin
                VC := C_Add(VC, C_Mul(TX[i_], a2[i_, j]));
              end;
            x[i2 + i, j2 + j] := VC;
            inc(j);
          end;
        inc(i);
      end;
  end;

  procedure RefCMatrixLeftTRSM(M: TLInt; n: TLInt; const a: TLComplexMatrix; i1: TLInt; j1: TLInt; IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt; var x: TLComplexMatrix; i2: TLInt; j2: TLInt);
  var
    a1: TLComplexMatrix;
    a2: TLComplexMatrix;
    TX: TLComplexVec;
    i: TLInt;
    j: TLInt;
    VC: TLComplex;
    RUpper: Boolean;
    i_: TLInt;
    i1_: TLInt;
  begin
    if n * M = 0 then
      begin
        Exit;
      end;
    SetLength(a1, M, M);
    i := 0;
    while i <= M - 1 do
      begin
        j := 0;
        while j <= M - 1 do
          begin
            a1[i, j] := C_Complex(0);
            inc(j);
          end;
        inc(i);
      end;
    if IsUpper then
      begin
        i := 0;
        while i <= M - 1 do
          begin
            j := i;
            while j <= M - 1 do
              begin
                a1[i, j] := a[i1 + i, j1 + j];
                inc(j);
              end;
            inc(i);
          end;
      end
    else
      begin
        i := 0;
        while i <= M - 1 do
          begin
            j := 0;
            while j <= i do
              begin
                a1[i, j] := a[i1 + i, j1 + j];
                inc(j);
              end;
            inc(i);
          end;
      end;
    RUpper := IsUpper;
    if IsUnit then
      begin
        i := 0;
        while i <= M - 1 do
          begin
            a1[i, i] := C_Complex(1);
            inc(i);
          end;
      end;
    SetLength(a2, M, M);
    if OpType = 0 then
      begin
        i := 0;
        while i <= M - 1 do
          begin
            j := 0;
            while j <= M - 1 do
              begin
                a2[i, j] := a1[i, j];
                inc(j);
              end;
            inc(i);
          end;
      end;
    if OpType = 1 then
      begin
        i := 0;
        while i <= M - 1 do
          begin
            j := 0;
            while j <= M - 1 do
              begin
                a2[i, j] := a1[j, i];
                inc(j);
              end;
            inc(i);
          end;
        RUpper := not RUpper;
      end;
    if OpType = 2 then
      begin
        i := 0;
        while i <= M - 1 do
          begin
            j := 0;
            while j <= M - 1 do
              begin
                a2[i, j] := Conj(a1[j, i]);
                inc(j);
              end;
            inc(i);
          end;
        RUpper := not RUpper;
      end;
    InternalCMatrixTRInverse(a2, M, RUpper, False);
    SetLength(TX, M);
    j := 0;
    while j <= n - 1 do
      begin
        i1_ := (i2) - (0);
        for i_ := 0 to M - 1 do
          begin
            TX[i_] := x[i_ + i1_, j2 + j];
          end;
        i := 0;
        while i <= M - 1 do
          begin
            VC := C_Complex(0.0);
            for i_ := 0 to M - 1 do
              begin
                VC := C_Add(VC, C_Mul(a2[i, i_], TX[i_]));
              end;
            x[i2 + i, j2 + j] := VC;
            inc(i);
          end;
        inc(j);
      end;
  end;

  procedure RefRMatrixRightTRSM(M: TLInt; n: TLInt; const a: TLMatrix; i1: TLInt; j1: TLInt; IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt; var x: TLMatrix; i2: TLInt; j2: TLInt);
  var
    a1: TLMatrix;
    a2: TLMatrix;
    TX: TLVec;
    i: TLInt;
    j: TLInt;
    vr: TLFloat;
    RUpper: Boolean;
    i_: TLInt;
  begin
    if n * M = 0 then
      begin
        Exit;
      end;
    SetLength(a1, n, n);
    i := 0;
    while i <= n - 1 do
      begin
        j := 0;
        while j <= n - 1 do
          begin
            a1[i, j] := 0;
            inc(j);
          end;
        inc(i);
      end;
    if IsUpper then
      begin
        i := 0;
        while i <= n - 1 do
          begin
            j := i;
            while j <= n - 1 do
              begin
                a1[i, j] := a[i1 + i, j1 + j];
                inc(j);
              end;
            inc(i);
          end;
      end
    else
      begin
        i := 0;
        while i <= n - 1 do
          begin
            j := 0;
            while j <= i do
              begin
                a1[i, j] := a[i1 + i, j1 + j];
                inc(j);
              end;
            inc(i);
          end;
      end;
    RUpper := IsUpper;
    if IsUnit then
      begin
        i := 0;
        while i <= n - 1 do
          begin
            a1[i, i] := 1;
            inc(i);
          end;
      end;
    SetLength(a2, n, n);
    if OpType = 0 then
      begin
        i := 0;
        while i <= n - 1 do
          begin
            j := 0;
            while j <= n - 1 do
              begin
                a2[i, j] := a1[i, j];
                inc(j);
              end;
            inc(i);
          end;
      end;
    if OpType = 1 then
      begin
        i := 0;
        while i <= n - 1 do
          begin
            j := 0;
            while j <= n - 1 do
              begin
                a2[i, j] := a1[j, i];
                inc(j);
              end;
            inc(i);
          end;
        RUpper := not RUpper;
      end;
    InternalRMatrixTRInverse(a2, n, RUpper, False);
    SetLength(TX, n);
    i := 0;
    while i <= M - 1 do
      begin
        APVMove(@TX[0], 0, n - 1, @x[i2 + i][0], j2, j2 + n - 1);
        j := 0;
        while j <= n - 1 do
          begin
            vr := 0.0;
            for i_ := 0 to n - 1 do
              begin
                vr := vr + TX[i_] * a2[i_, j];
              end;
            x[i2 + i, j2 + j] := vr;
            inc(j);
          end;
        inc(i);
      end;
  end;

  procedure RefRMatrixLeftTRSM(M: TLInt; n: TLInt; const a: TLMatrix; i1: TLInt; j1: TLInt; IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt; var x: TLMatrix; i2: TLInt; j2: TLInt);
  var
    a1: TLMatrix;
    a2: TLMatrix;
    TX: TLVec;
    i: TLInt;
    j: TLInt;
    vr: TLFloat;
    RUpper: Boolean;
    i_: TLInt;
    i1_: TLInt;
  begin
    if n * M = 0 then
      begin
        Exit;
      end;
    SetLength(a1, M, M);
    i := 0;
    while i <= M - 1 do
      begin
        j := 0;
        while j <= M - 1 do
          begin
            a1[i, j] := 0;
            inc(j);
          end;
        inc(i);
      end;
    if IsUpper then
      begin
        i := 0;
        while i <= M - 1 do
          begin
            j := i;
            while j <= M - 1 do
              begin
                a1[i, j] := a[i1 + i, j1 + j];
                inc(j);
              end;
            inc(i);
          end;
      end
    else
      begin
        i := 0;
        while i <= M - 1 do
          begin
            j := 0;
            while j <= i do
              begin
                a1[i, j] := a[i1 + i, j1 + j];
                inc(j);
              end;
            inc(i);
          end;
      end;
    RUpper := IsUpper;
    if IsUnit then
      begin
        i := 0;
        while i <= M - 1 do
          begin
            a1[i, i] := 1;
            inc(i);
          end;
      end;
    SetLength(a2, M, M);
    if OpType = 0 then
      begin
        i := 0;
        while i <= M - 1 do
          begin
            j := 0;
            while j <= M - 1 do
              begin
                a2[i, j] := a1[i, j];
                inc(j);
              end;
            inc(i);
          end;
      end;
    if OpType = 1 then
      begin
        i := 0;
        while i <= M - 1 do
          begin
            j := 0;
            while j <= M - 1 do
              begin
                a2[i, j] := a1[j, i];
                inc(j);
              end;
            inc(i);
          end;
        RUpper := not RUpper;
      end;
    InternalRMatrixTRInverse(a2, M, RUpper, False);
    SetLength(TX, M);
    j := 0;
    while j <= n - 1 do
      begin
        i1_ := (i2) - (0);
        for i_ := 0 to M - 1 do
          begin
            TX[i_] := x[i_ + i1_, j2 + j];
          end;
        i := 0;
        while i <= M - 1 do
          begin
            vr := APVDotProduct(@a2[i][0], 0, M - 1, @TX[0], 0, M - 1);
            x[i2 + i, j2 + j] := vr;
            inc(i);
          end;
        inc(j);
      end;
  end;

  procedure RefCMatrixSYRK(n: TLInt; k: TLInt; alpha: TLFloat; const a: TLComplexMatrix; IA: TLInt; ja: TLInt; OpTypeA: TLInt; beta: TLFloat; var c: TLComplexMatrix; IC: TLInt; JC: TLInt; IsUpper: Boolean);
  var
    AE: TLComplexMatrix;
    i: TLInt;
    j: TLInt;
    VC: TLComplex;
    i_: TLInt;
  begin
    i := 0;
    while i <= n - 1 do
      begin
        j := 0;
        while j <= n - 1 do
          begin
            if IsUpper and (j >= i) or not IsUpper and (j <= i) then
              begin
                if AP_FP_Eq(beta, 0) then
                  begin
                    c[i + IC, j + JC] := C_Complex(0);
                  end
                else
                  begin
                    c[i + IC, j + JC] := C_MulR(c[i + IC, j + JC], beta);
                  end;
              end;
            inc(j);
          end;
        inc(i);
      end;
    if AP_FP_Eq(alpha, 0) then
      begin
        Exit;
      end;
    if n * k > 0 then
      begin
        SetLength(AE, n, k);
      end;
    i := 0;
    while i <= n - 1 do
      begin
        j := 0;
        while j <= k - 1 do
          begin
            if OpTypeA = 0 then
              begin
                AE[i, j] := a[IA + i, ja + j];
              end;
            if OpTypeA = 2 then
              begin
                AE[i, j] := Conj(a[IA + j, ja + i]);
              end;
            inc(j);
          end;
        inc(i);
      end;
    i := 0;
    while i <= n - 1 do
      begin
        j := 0;
        while j <= n - 1 do
          begin
            VC := C_Complex(0);
            if k > 0 then
              begin
                VC := C_Complex(0.0);
                for i_ := 0 to k - 1 do
                  begin
                    VC := C_Add(VC, C_Mul(AE[i, i_], Conj(AE[j, i_])));
                  end;
              end;
            VC := C_MulR(VC, alpha);
            if IsUpper and (j >= i) then
              begin
                c[IC + i, JC + j] := C_Add(VC, c[IC + i, JC + j]);
              end;
            if not IsUpper and (j <= i) then
              begin
                c[IC + i, JC + j] := C_Add(VC, c[IC + i, JC + j]);
              end;
            inc(j);
          end;
        inc(i);
      end;
  end;

  procedure RefRMatrixSYRK(n: TLInt; k: TLInt; alpha: TLFloat; const a: TLMatrix; IA: TLInt; ja: TLInt; OpTypeA: TLInt; beta: TLFloat; var c: TLMatrix; IC: TLInt; JC: TLInt; IsUpper: Boolean);
  var
    AE: TLMatrix;
    i: TLInt;
    j: TLInt;
    vr: TLFloat;
  begin
    i := 0;
    while i <= n - 1 do
      begin
        j := 0;
        while j <= n - 1 do
          begin
            if IsUpper and (j >= i) or not IsUpper and (j <= i) then
              begin
                if AP_FP_Eq(beta, 0) then
                  begin
                    c[i + IC, j + JC] := 0;
                  end
                else
                  begin
                    c[i + IC, j + JC] := c[i + IC, j + JC] * beta;
                  end;
              end;
            inc(j);
          end;
        inc(i);
      end;
    if AP_FP_Eq(alpha, 0) then
      begin
        Exit;
      end;
    if n * k > 0 then
      begin
        SetLength(AE, n, k);
      end;
    i := 0;
    while i <= n - 1 do
      begin
        j := 0;
        while j <= k - 1 do
          begin
            if OpTypeA = 0 then
              begin
                AE[i, j] := a[IA + i, ja + j];
              end;
            if OpTypeA = 1 then
              begin
                AE[i, j] := a[IA + j, ja + i];
              end;
            inc(j);
          end;
        inc(i);
      end;
    i := 0;
    while i <= n - 1 do
      begin
        j := 0;
        while j <= n - 1 do
          begin
            vr := 0;
            if k > 0 then
              begin
                vr := APVDotProduct(@AE[i][0], 0, k - 1, @AE[j][0], 0, k - 1);
              end;
            vr := alpha * vr;
            if IsUpper and (j >= i) then
              begin
                c[IC + i, JC + j] := vr + c[IC + i, JC + j];
              end;
            if not IsUpper and (j <= i) then
              begin
                c[IC + i, JC + j] := vr + c[IC + i, JC + j];
              end;
            inc(j);
          end;
        inc(i);
      end;
  end;

  procedure RefCMatrixGEMM(M: TLInt; n: TLInt; k: TLInt;
    alpha: TLComplex; const a: TLComplexMatrix; IA: TLInt;
    ja: TLInt; OpTypeA: TLInt; const b: TLComplexMatrix;
    IB: TLInt; JB: TLInt; OpTypeB: TLInt; beta: TLComplex;
    var c: TLComplexMatrix; IC: TLInt; JC: TLInt);
  var
    AE: TLComplexMatrix;
    BE: TLComplexMatrix;
    i: TLInt;
    j: TLInt;
    VC: TLComplex;
    i_: TLInt;
  begin
    SetLength(AE, M, k);
    i := 0;
    while i <= M - 1 do
      begin
        j := 0;
        while j <= k - 1 do
          begin
            if OpTypeA = 0 then
              begin
                AE[i, j] := a[IA + i, ja + j];
              end;
            if OpTypeA = 1 then
              begin
                AE[i, j] := a[IA + j, ja + i];
              end;
            if OpTypeA = 2 then
              begin
                AE[i, j] := Conj(a[IA + j, ja + i]);
              end;
            inc(j);
          end;
        inc(i);
      end;
    SetLength(BE, k, n);
    i := 0;
    while i <= k - 1 do
      begin
        j := 0;
        while j <= n - 1 do
          begin
            if OpTypeB = 0 then
              begin
                BE[i, j] := b[IB + i, JB + j];
              end;
            if OpTypeB = 1 then
              begin
                BE[i, j] := b[IB + j, JB + i];
              end;
            if OpTypeB = 2 then
              begin
                BE[i, j] := Conj(b[IB + j, JB + i]);
              end;
            inc(j);
          end;
        inc(i);
      end;
    i := 0;
    while i <= M - 1 do
      begin
        j := 0;
        while j <= n - 1 do
          begin
            VC := C_Complex(0.0);
            for i_ := 0 to k - 1 do
              begin
                VC := C_Add(VC, C_Mul(AE[i, i_], BE[i_, j]));
              end;
            VC := C_Mul(alpha, VC);
            if C_NotEqualR(beta, 0) then
              begin
                VC := C_Add(VC, C_Mul(beta, c[IC + i, JC + j]));
              end;
            c[IC + i, JC + j] := VC;
            inc(j);
          end;
        inc(i);
      end;
  end;

  procedure RefRMatrixGEMM(M: TLInt; n: TLInt; k: TLInt;
    alpha: TLFloat; const a: TLMatrix; IA: TLInt;
    ja: TLInt; OpTypeA: TLInt; const b: TLMatrix;
    IB: TLInt; JB: TLInt; OpTypeB: TLInt;
    beta: TLFloat; var c: TLMatrix; IC: TLInt; JC: TLInt);
  var
    AE: TLMatrix;
    BE: TLMatrix;
    i: TLInt;
    j: TLInt;
    VC: TLFloat;
    i_: TLInt;
  begin
    SetLength(AE, M, k);
    i := 0;
    while i <= M - 1 do
      begin
        j := 0;
        while j <= k - 1 do
          begin
            if OpTypeA = 0 then
              begin
                AE[i, j] := a[IA + i, ja + j];
              end;
            if OpTypeA = 1 then
              begin
                AE[i, j] := a[IA + j, ja + i];
              end;
            inc(j);
          end;
        inc(i);
      end;
    SetLength(BE, k, n);
    i := 0;
    while i <= k - 1 do
      begin
        j := 0;
        while j <= n - 1 do
          begin
            if OpTypeB = 0 then
              begin
                BE[i, j] := b[IB + i, JB + j];
              end;
            if OpTypeB = 1 then
              begin
                BE[i, j] := b[IB + j, JB + i];
              end;
            inc(j);
          end;
        inc(i);
      end;
    i := 0;
    while i <= M - 1 do
      begin
        j := 0;
        while j <= n - 1 do
          begin
            VC := 0.0;
            for i_ := 0 to k - 1 do
              begin
                VC := VC + AE[i, i_] * BE[i_, j];
              end;
            VC := alpha * VC;
            if AP_FP_NEq(beta, 0) then
              begin
                VC := VC + beta * c[IC + i, JC + j];
              end;
            c[IC + i, JC + j] := VC;
            inc(j);
          end;
        inc(i);
      end;
  end;

  procedure NaiveMatrixMatrixMultiply(const a: TLMatrix; AI1: TLInt;
    AI2: TLInt; AJ1: TLInt; AJ2: TLInt; TransA: Boolean;
    const b: TLMatrix; BI1: TLInt; BI2: TLInt;
    BJ1: TLInt; BJ2: TLInt; TransB: Boolean; alpha: TLFloat;
    var c: TLMatrix; CI1: TLInt; CI2: TLInt;
    CJ1: TLInt; CJ2: TLInt; beta: TLFloat);
  var
    ARows: TLInt;
    ACols: TLInt;
    BRows: TLInt;
    BCols: TLInt;
    i: TLInt;
    j: TLInt;
    k: TLInt;
    L: TLInt;
    r: TLInt;
    v: TLFloat;
    x1: TLVec;
    x2: TLVec;
    i_: TLInt;
    i1_: TLInt;
  begin

    //
    // Setup
    //
    if not TransA then
      begin
        ARows := AI2 - AI1 + 1;
        ACols := AJ2 - AJ1 + 1;
      end
    else
      begin
        ARows := AJ2 - AJ1 + 1;
        ACols := AI2 - AI1 + 1;
      end;
    if not TransB then
      begin
        BRows := BI2 - BI1 + 1;
        BCols := BJ2 - BJ1 + 1;
      end
    else
      begin
        BRows := BJ2 - BJ1 + 1;
        BCols := BI2 - BI1 + 1;
      end;
    Assert(ACols = BRows, 'NaiveMatrixMatrixMultiply: incorrect matrix sizes!');
    if (ARows <= 0) or (ACols <= 0) or (BRows <= 0) or (BCols <= 0) then
      begin
        Exit;
      end;
    L := ARows;
    r := BCols;
    k := ACols;
    SetLength(x1, k + 1);
    SetLength(x2, k + 1);
    i := 1;
    while i <= L do
      begin
        j := 1;
        while j <= r do
          begin
            if not TransA then
              begin
                if not TransB then
                  begin
                    i1_ := (AJ1) - (BI1);
                    v := 0.0;
                    for i_ := BI1 to BI2 do
                      begin
                        v := v + b[i_, BJ1 + j - 1] * a[AI1 + i - 1, i_ + i1_];
                      end;
                  end
                else
                  begin
                    v := APVDotProduct(@b[BI1 + j - 1][0], BJ1, BJ2, @a[AI1 + i - 1][0], AJ1, AJ2);
                  end;
              end
            else
              begin
                if not TransB then
                  begin
                    i1_ := (AI1) - (BI1);
                    v := 0.0;
                    for i_ := BI1 to BI2 do
                      begin
                        v := v + b[i_, BJ1 + j - 1] * a[i_ + i1_, AJ1 + i - 1];
                      end;
                  end
                else
                  begin
                    i1_ := (AI1) - (BJ1);
                    v := 0.0;
                    for i_ := BJ1 to BJ2 do
                      begin
                        v := v + b[BI1 + j - 1, i_] * a[i_ + i1_, AJ1 + i - 1];
                      end;
                  end;
              end;
            if AP_FP_Eq(beta, 0) then
              begin
                c[CI1 + i - 1, CJ1 + j - 1] := alpha * v;
              end
            else
              begin
                c[CI1 + i - 1, CJ1 + j - 1] := beta * c[CI1 + i - 1, CJ1 + j - 1] + alpha * v;
              end;
            inc(j);
          end;
        inc(i);
      end;
  end;

  function TestTRSM(MinN: TLInt; MaxN: TLInt): Boolean;
  var
    n: TLInt;
    M: TLInt;
    mx: TLInt;
    i: TLInt;
    j: TLInt;
    OpType: TLInt;
    UpperType: TLInt;
    UnitType: TLInt;
    XOffsI: TLInt;
    XOffsJ: TLInt;
    AOffsIType: TLInt;
    AOffsJType: TLInt;
    AOffsI: TLInt;
    AOffsJ: TLInt;
    RefRA: TLMatrix;
    RefRXL: TLMatrix;
    RefRXR: TLMatrix;
    RefCA: TLComplexMatrix;
    RefCXL: TLComplexMatrix;
    RefCXR: TLComplexMatrix;
    ResArry: TLMatrix;
    ca: TLComplexMatrix;
    RXR1: TLMatrix;
    RXL1: TLMatrix;
    CXR1: TLComplexMatrix;
    CXL1: TLComplexMatrix;
    RXR2: TLMatrix;
    RXL2: TLMatrix;
    CXR2: TLComplexMatrix;
    CXL2: TLComplexMatrix;
    Threshold: TLFloat;
  begin
    Threshold := AP_Sqr(MaxN) * 100 * MachineEpsilon;
    Result := False;
    mx := MinN;
    while mx <= MaxN do
      begin

        //
        // Select random M/N in [1,MX] such that max(M,N)=MX
        //
        M := 1 + RandomInteger(mx);
        n := 1 + RandomInteger(mx);
        if AP_FP_Greater(RandomReal, 0.5) then
          begin
            M := mx;
          end
        else
          begin
            n := mx;
          end;

        //
        // Initialize RefRA/RefCA by random matrices whose upper
        // and lower triangle submatrices are non-degenerate
        // well-conditioned matrices.
        //
        // Matrix size is 2Mx2M (four copies of same MxM matrix
        // to test different offsets)
        //
        SetLength(RefRA, 2 * M, 2 * M);
        i := 0;
        while i <= M - 1 do
          begin
            j := 0;
            while j <= M - 1 do
              begin
                RefRA[i, j] := 0.2 * RandomReal - 0.1;
                inc(j);
              end;
            inc(i);
          end;
        i := 0;
        while i <= M - 1 do
          begin
            RefRA[i, i] := (2 * RandomInteger(1) - 1) * (2 * M + RandomReal);
            inc(i);
          end;
        i := 0;
        while i <= M - 1 do
          begin
            j := 0;
            while j <= M - 1 do
              begin
                RefRA[i + M, j] := RefRA[i, j];
                RefRA[i, j + M] := RefRA[i, j];
                RefRA[i + M, j + M] := RefRA[i, j];
                inc(j);
              end;
            inc(i);
          end;
        SetLength(RefCA, 2 * M, 2 * M);
        i := 0;
        while i <= M - 1 do
          begin
            j := 0;
            while j <= M - 1 do
              begin
                RefCA[i, j].x := 0.2 * RandomReal - 0.1;
                RefCA[i, j].y := 0.2 * RandomReal - 0.1;
                inc(j);
              end;
            inc(i);
          end;
        i := 0;
        while i <= M - 1 do
          begin
            RefCA[i, i].x := (2 * RandomInteger(2) - 1) * (2 * M + RandomReal);
            RefCA[i, i].y := (2 * RandomInteger(2) - 1) * (2 * M + RandomReal);
            inc(i);
          end;
        i := 0;
        while i <= M - 1 do
          begin
            j := 0;
            while j <= M - 1 do
              begin
                RefCA[i + M, j] := RefCA[i, j];
                RefCA[i, j + M] := RefCA[i, j];
                RefCA[i + M, j + M] := RefCA[i, j];
                inc(j);
              end;
            inc(i);
          end;

        //
        // Generate random XL/XR.
        //
        // XR is NxM matrix (matrix for 'Right' subroutines)
        // XL is MxN matrix (matrix for 'Left' subroutines)
        //
        SetLength(RefRXR, n, M);
        i := 0;
        while i <= n - 1 do
          begin
            j := 0;
            while j <= M - 1 do
              begin
                RefRXR[i, j] := 2 * RandomReal - 1;
                inc(j);
              end;
            inc(i);
          end;
        SetLength(RefRXL, M, n);
        i := 0;
        while i <= M - 1 do
          begin
            j := 0;
            while j <= n - 1 do
              begin
                RefRXL[i, j] := 2 * RandomReal - 1;
                inc(j);
              end;
            inc(i);
          end;
        SetLength(RefCXR, n, M);
        i := 0;
        while i <= n - 1 do
          begin
            j := 0;
            while j <= M - 1 do
              begin
                RefCXR[i, j].x := 2 * RandomReal - 1;
                RefCXR[i, j].y := 2 * RandomReal - 1;
                inc(j);
              end;
            inc(i);
          end;
        SetLength(RefCXL, M, n);
        i := 0;
        while i <= M - 1 do
          begin
            j := 0;
            while j <= n - 1 do
              begin
                RefCXL[i, j].x := 2 * RandomReal - 1;
                RefCXL[i, j].y := 2 * RandomReal - 1;
                inc(j);
              end;
            inc(i);
          end;

        //
        // test different types of operations, offsets, and so on...
        //
        // to avoid unnecessary slowdown we don't test ALL possible
        // combinations of operation types. We just generate one random
        // set of parameters and test it.
        //
        SetLength(ResArry, 2 * M, 2 * M);
        SetLength(RXR1, n, M);
        SetLength(RXR2, n, M);
        SetLength(RXL1, M, n);
        SetLength(RXL2, M, n);
        SetLength(ca, 2 * M, 2 * M);
        SetLength(CXR1, n, M);
        SetLength(CXR2, n, M);
        SetLength(CXL1, M, n);
        SetLength(CXL2, M, n);
        OpType := RandomInteger(3);
        UpperType := RandomInteger(2);
        UnitType := RandomInteger(2);
        XOffsI := RandomInteger(2);
        XOffsJ := RandomInteger(2);
        AOffsIType := RandomInteger(2);
        AOffsJType := RandomInteger(2);
        AOffsI := M * AOffsIType;
        AOffsJ := M * AOffsJType;

        //
        // copy A, XR, XL (fill unused parts with random garbage)
        //
        i := 0;
        while i <= 2 * M - 1 do
          begin
            j := 0;
            while j <= 2 * M - 1 do
              begin
                if (i >= AOffsI) and (i < AOffsI + M) and (j >= AOffsJ) and
                  (j < AOffsJ + M) then
                  begin
                    ca[i, j] := RefCA[i, j];
                    ResArry[i, j] := RefRA[i, j];
                  end
                else
                  begin
                    ca[i, j] := C_Complex(RandomReal);
                    ResArry[i, j] := RandomReal;
                  end;
                inc(j);
              end;
            inc(i);
          end;
        i := 0;
        while i <= n - 1 do
          begin
            j := 0;
            while j <= M - 1 do
              begin
                if (i >= XOffsI) and (j >= XOffsJ) then
                  begin
                    CXR1[i, j] := RefCXR[i, j];
                    CXR2[i, j] := RefCXR[i, j];
                    RXR1[i, j] := RefRXR[i, j];
                    RXR2[i, j] := RefRXR[i, j];
                  end
                else
                  begin
                    CXR1[i, j] := C_Complex(RandomReal);
                    CXR2[i, j] := CXR1[i, j];
                    RXR1[i, j] := RandomReal;
                    RXR2[i, j] := RXR1[i, j];
                  end;
                inc(j);
              end;
            inc(i);
          end;
        i := 0;
        while i <= M - 1 do
          begin
            j := 0;
            while j <= n - 1 do
              begin
                if (i >= XOffsI) and (j >= XOffsJ) then
                  begin
                    CXL1[i, j] := RefCXL[i, j];
                    CXL2[i, j] := RefCXL[i, j];
                    RXL1[i, j] := RefRXL[i, j];
                    RXL2[i, j] := RefRXL[i, j];
                  end
                else
                  begin
                    CXL1[i, j] := C_Complex(RandomReal);
                    CXL2[i, j] := CXL1[i, j];
                    RXL1[i, j] := RandomReal;
                    RXL2[i, j] := RXL1[i, j];
                  end;
                inc(j);
              end;
            inc(i);
          end;

        //
        // Test CXR
        //
        CMatrixRightTRSM(n - XOffsI, M - XOffsJ, ca, AOffsI, AOffsJ, UpperType = 0, UnitType = 0, OpType, CXR1, XOffsI, XOffsJ);
        RefCMatrixRightTRSM(n - XOffsI, M - XOffsJ, ca, AOffsI, AOffsJ, UpperType = 0, UnitType = 0, OpType, CXR2, XOffsI, XOffsJ);
        i := 0;
        while i <= n - 1 do
          begin
            j := 0;
            while j <= M - 1 do
              begin
                Result := Result or
                  AP_FP_Greater(AbsComplex(C_Sub(CXR1[i, j], CXR2[i, j])), Threshold);
                inc(j);
              end;
            inc(i);
          end;

        //
        // Test CXL
        //
        CMatrixLeftTRSM(M - XOffsI, n - XOffsJ, ca, AOffsI, AOffsJ, UpperType = 0, UnitType = 0, OpType, CXL1, XOffsI, XOffsJ);
        RefCMatrixLeftTRSM(M - XOffsI, n - XOffsJ, ca, AOffsI, AOffsJ, UpperType = 0, UnitType = 0, OpType, CXL2, XOffsI, XOffsJ);
        i := 0;
        while i <= M - 1 do
          begin
            j := 0;
            while j <= n - 1 do
              begin
                Result := Result or AP_FP_Greater(AbsComplex(C_Sub(CXL1[i, j], CXL2[i, j])), Threshold);
                inc(j);
              end;
            inc(i);
          end;
        if OpType < 2 then
          begin

            //
            // Test RXR
            //
            RMatrixRightTRSM(n - XOffsI, M - XOffsJ, ResArry, AOffsI, AOffsJ, UpperType = 0, UnitType = 0, OpType, RXR1, XOffsI, XOffsJ);
            RefRMatrixRightTRSM(n - XOffsI, M - XOffsJ, ResArry, AOffsI, AOffsJ, UpperType = 0, UnitType = 0, OpType, RXR2, XOffsI, XOffsJ);
            i := 0;
            while i <= n - 1 do
              begin
                j := 0;
                while j <= M - 1 do
                  begin
                    Result := Result or AP_FP_Greater(AbsReal(RXR1[i, j] - RXR2[i, j]),
                      Threshold);
                    inc(j);
                  end;
                inc(i);
              end;

            //
            // Test RXL
            //
            RMatrixLeftTRSM(M - XOffsI, n - XOffsJ, ResArry, AOffsI, AOffsJ, UpperType = 0, UnitType = 0, OpType, RXL1, XOffsI, XOffsJ);
            RefRMatrixLeftTRSM(M - XOffsI, n - XOffsJ, ResArry, AOffsI, AOffsJ, UpperType = 0, UnitType = 0, OpType, RXL2, XOffsI, XOffsJ);
            i := 0;
            while i <= M - 1 do
              begin
                j := 0;
                while j <= n - 1 do
                  begin
                    Result := Result or AP_FP_Greater(AbsReal(RXL1[i, j] - RXL2[i, j]), Threshold);
                    inc(j);
                  end;
                inc(i);
              end;
          end;
        inc(mx);
      end;
  end;

  function TestSYRK(MinN: TLInt; MaxN: TLInt): Boolean;
  var
    n: TLInt;
    k: TLInt;
    mx: TLInt;
    i: TLInt;
    j: TLInt;
    UpperType: TLInt;
    XOffsI: TLInt;
    XOffsJ: TLInt;
    AOffsIType: TLInt;
    AOffsJType: TLInt;
    AOffsI: TLInt;
    AOffsJ: TLInt;
    AlphaType: TLInt;
    BetaType: TLInt;
    RefRA: TLMatrix;
    RefRC: TLMatrix;
    RefCA: TLComplexMatrix;
    RefCC: TLComplexMatrix;
    alpha: TLFloat;
    beta: TLFloat;
    RA1: TLMatrix;
    RA2: TLMatrix;
    CA1: TLComplexMatrix;
    CA2: TLComplexMatrix;
    RC: TLMatrix;
    RCT: TLMatrix;
    CC: TLComplexMatrix;
    CCT: TLComplexMatrix;
    Threshold: TLFloat;
  begin
    Threshold := MaxN * 100 * MachineEpsilon;
    Result := False;
    mx := MinN;
    while mx <= MaxN do
      begin

        //
        // Select random M/N in [1,MX] such that max(M,N)=MX
        //
        k := 1 + RandomInteger(mx);
        n := 1 + RandomInteger(mx);
        if AP_FP_Greater(RandomReal, 0.5) then
          begin
            k := mx;
          end
        else
          begin
            n := mx;
          end;

        //
        // Initialize RefRA/RefCA by random Hermitian matrices,
        // RefRC/RefCC by random matrices
        //
        // ResArry/CA size is 2Nx2N (four copies of same NxN matrix
        // to test different offsets)
        //
        SetLength(RefRA, 2 * n, 2 * n);
        SetLength(RefCA, 2 * n, 2 * n);
        i := 0;
        while i <= n - 1 do
          begin
            RefRA[i, i] := 2 * RandomReal - 1;
            RefCA[i, i] := C_Complex(2 * RandomReal - 1);
            j := i + 1;
            while j <= n - 1 do
              begin
                RefRA[i, j] := 2 * RandomReal - 1;
                RefCA[i, j].x := 2 * RandomReal - 1;
                RefCA[i, j].y := 2 * RandomReal - 1;
                RefRA[j, i] := RefRA[i, j];
                RefCA[j, i] := Conj(RefCA[i, j]);
                inc(j);
              end;
            inc(i);
          end;
        i := 0;
        while i <= n - 1 do
          begin
            j := 0;
            while j <= n - 1 do
              begin
                RefRA[i + n, j] := RefRA[i, j];
                RefRA[i, j + n] := RefRA[i, j];
                RefRA[i + n, j + n] := RefRA[i, j];
                RefCA[i + n, j] := RefCA[i, j];
                RefCA[i, j + n] := RefCA[i, j];
                RefCA[i + n, j + n] := RefCA[i, j];
                inc(j);
              end;
            inc(i);
          end;
        SetLength(RefRC, n, k);
        SetLength(RefCC, n, k);
        i := 0;
        while i <= n - 1 do
          begin
            j := 0;
            while j <= k - 1 do
              begin
                RefRC[i, j] := 2 * RandomReal - 1;
                RefCC[i, j].x := 2 * RandomReal - 1;
                RefCC[i, j].y := 2 * RandomReal - 1;
                inc(j);
              end;
            inc(i);
          end;

        //
        // test different types of operations, offsets, and so on...
        //
        // to avoid unnecessary slowdown we don't test ALL possible
        // combinations of operation types. We just generate one random
        // set of parameters and test it.
        //
        SetLength(RA1, 2 * n, 2 * n);
        SetLength(RA2, 2 * n, 2 * n);
        SetLength(CA1, 2 * n, 2 * n);
        SetLength(CA2, 2 * n, 2 * n);
        SetLength(RC, n, k);
        SetLength(RCT, k, n);
        SetLength(CC, n, k);
        SetLength(CCT, k, n);
        UpperType := RandomInteger(2);
        XOffsI := RandomInteger(2);
        XOffsJ := RandomInteger(2);
        AOffsIType := RandomInteger(2);
        AOffsJType := RandomInteger(2);
        AlphaType := RandomInteger(2);
        BetaType := RandomInteger(2);
        AOffsI := n * AOffsIType;
        AOffsJ := n * AOffsJType;
        alpha := AlphaType * (2 * RandomReal - 1);
        beta := BetaType * (2 * RandomReal - 1);

        //
        // copy A, C (fill unused parts with random garbage)
        //
        i := 0;
        while i <= 2 * n - 1 do
          begin
            j := 0;
            while j <= 2 * n - 1 do
              begin
                if (i >= AOffsI) and (i < AOffsI + n) and (j >= AOffsJ) and
                  (j < AOffsJ + n) then
                  begin
                    CA1[i, j] := RefCA[i, j];
                    CA2[i, j] := RefCA[i, j];
                    RA1[i, j] := RefRA[i, j];
                    RA2[i, j] := RefRA[i, j];
                  end
                else
                  begin
                    CA1[i, j] := C_Complex(RandomReal);
                    CA2[i, j] := CA1[i, j];
                    RA1[i, j] := RandomReal;
                    RA2[i, j] := RA1[i, j];
                  end;
                inc(j);
              end;
            inc(i);
          end;
        i := 0;
        while i <= n - 1 do
          begin
            j := 0;
            while j <= k - 1 do
              begin
                if (i >= XOffsI) and (j >= XOffsJ) then
                  begin
                    RC[i, j] := RefRC[i, j];
                    RCT[j, i] := RefRC[i, j];
                    CC[i, j] := RefCC[i, j];
                    CCT[j, i] := RefCC[i, j];
                  end
                else
                  begin
                    RC[i, j] := RandomReal;
                    RCT[j, i] := RC[i, j];
                    CC[i, j] := C_Complex(RandomReal);
                    CCT[j, i] := CCT[j, i];
                  end;
                inc(j);
              end;
            inc(i);
          end;

        //
        // Test TLComplex
        // Only one of transform types is selected and tested
        //
        if AP_FP_Greater(RandomReal, 0.5) then
          begin
            CMatrixSYRK(n - XOffsI, k - XOffsJ, alpha, CC, XOffsI, XOffsJ, 0, beta, CA1, AOffsI, AOffsJ, UpperType = 0);
            RefCMatrixSYRK(n - XOffsI, k - XOffsJ, alpha, CC, XOffsI, XOffsJ, 0, beta, CA2, AOffsI, AOffsJ, UpperType = 0);
          end
        else
          begin
            CMatrixSYRK(n - XOffsI, k - XOffsJ, alpha, CCT, XOffsJ, XOffsI, 2, beta, CA1, AOffsI, AOffsJ, UpperType = 0);
            RefCMatrixSYRK(n - XOffsI, k - XOffsJ, alpha, CCT, XOffsJ, XOffsI, 2, beta, CA2, AOffsI, AOffsJ, UpperType = 0);
          end;
        i := 0;
        while i <= n - 1 do
          begin
            j := 0;
            while j <= n - 1 do
              begin
                Result := Result or AP_FP_Greater(AbsComplex(C_Sub(CA1[i, j], CA2[i, j])), Threshold);
                inc(j);
              end;
            inc(i);
          end;

        //
        // Test real
        // Only one of transform types is selected and tested
        //
        if AP_FP_Greater(RandomReal, 0.5) then
          begin
            RMatrixSYRK(n - XOffsI, k - XOffsJ, alpha, RC, XOffsI, XOffsJ, 0, beta, RA1, AOffsI, AOffsJ, UpperType = 0);
            RefRMatrixSYRK(n - XOffsI, k - XOffsJ, alpha, RC, XOffsI, XOffsJ, 0, beta, RA2, AOffsI, AOffsJ, UpperType = 0);
          end
        else
          begin
            RMatrixSYRK(n - XOffsI, k - XOffsJ, alpha, RCT, XOffsJ, XOffsI, 1, beta, RA1, AOffsI, AOffsJ, UpperType = 0);
            RefRMatrixSYRK(n - XOffsI, k - XOffsJ, alpha, RCT, XOffsJ, XOffsI, 1, beta, RA2, AOffsI, AOffsJ, UpperType = 0);
          end;
        i := 0;
        while i <= n - 1 do
          begin
            j := 0;
            while j <= n - 1 do
              begin
                Result := Result or AP_FP_Greater(AbsReal(RA1[i, j] - RA2[i, j]), Threshold);
                inc(j);
              end;
            inc(i);
          end;
        inc(mx);
      end;
  end;

  function TestGEMM(MinN: TLInt; MaxN: TLInt): Boolean;
  var
    M: TLInt;
    n: TLInt;
    k: TLInt;
    mx: TLInt;
    i: TLInt;
    j: TLInt;
    AOffsI: TLInt;
    AOffsJ: TLInt;
    AOpType: TLInt;
    AOpTypeR: TLInt;
    BOffsI: TLInt;
    BOffsJ: TLInt;
    BOpType: TLInt;
    BOpTypeR: TLInt;
    COffsI: TLInt;
    COffsJ: TLInt;
    RefRA: TLMatrix;
    RefRB: TLMatrix;
    RefRC: TLMatrix;
    RefCA: TLComplexMatrix;
    RefCB: TLComplexMatrix;
    RefCC: TLComplexMatrix;
    AlphaR: TLFloat;
    BetaR: TLFloat;
    alphaC: TLComplex;
    betaC: TLComplex;
    RC1: TLMatrix;
    RC2: TLMatrix;
    CC1: TLComplexMatrix;
    CC2: TLComplexMatrix;
    Threshold: TLFloat;
  begin
    Threshold := MaxN * 100 * MachineEpsilon;
    Result := False;
    mx := MinN;
    while mx <= MaxN do
      begin

        //
        // Select random M/N/K in [1,MX] such that max(M,N,K)=MX
        //
        M := 1 + RandomInteger(mx);
        n := 1 + RandomInteger(mx);
        k := 1 + RandomInteger(mx);
        i := RandomInteger(3);
        if i = 0 then
          begin
            M := mx;
          end;
        if i = 1 then
          begin
            n := mx;
          end;
        if i = 2 then
          begin
            k := mx;
          end;

        //
        // Initialize A/B/C by random matrices with size (MaxN+1)*(MaxN+1)
        //
        SetLength(RefRA, MaxN + 1, MaxN + 1);
        SetLength(RefRB, MaxN + 1, MaxN + 1);
        SetLength(RefRC, MaxN + 1, MaxN + 1);
        SetLength(RefCA, MaxN + 1, MaxN + 1);
        SetLength(RefCB, MaxN + 1, MaxN + 1);
        SetLength(RefCC, MaxN + 1, MaxN + 1);
        i := 0;
        while i <= MaxN do
          begin
            j := 0;
            while j <= MaxN do
              begin
                RefRA[i, j] := 2 * RandomReal - 1;
                RefRB[i, j] := 2 * RandomReal - 1;
                RefRC[i, j] := 2 * RandomReal - 1;
                RefCA[i, j].x := 2 * RandomReal - 1;
                RefCA[i, j].y := 2 * RandomReal - 1;
                RefCB[i, j].x := 2 * RandomReal - 1;
                RefCB[i, j].y := 2 * RandomReal - 1;
                RefCC[i, j].x := 2 * RandomReal - 1;
                RefCC[i, j].y := 2 * RandomReal - 1;
                inc(j);
              end;
            inc(i);
          end;

        //
        // test different types of operations, offsets, and so on...
        //
        // to avoid unnecessary slowdown we don't test ALL possible
        // combinations of operation types. We just generate one random
        // set of parameters and test it.
        //
        SetLength(RC1, MaxN + 1, MaxN + 1);
        SetLength(RC2, MaxN + 1, MaxN + 1);
        SetLength(CC1, MaxN + 1, MaxN + 1);
        SetLength(CC2, MaxN + 1, MaxN + 1);
        AOffsI := RandomInteger(2);
        AOffsJ := RandomInteger(2);
        AOpType := RandomInteger(3);
        AOpTypeR := RandomInteger(2);
        BOffsI := RandomInteger(2);
        BOffsJ := RandomInteger(2);
        BOpType := RandomInteger(3);
        BOpTypeR := RandomInteger(2);
        COffsI := RandomInteger(2);
        COffsJ := RandomInteger(2);
        AlphaR := RandomInteger(2) * (2 * RandomReal - 1);
        BetaR := RandomInteger(2) * (2 * RandomReal - 1);
        if AP_FP_Greater(RandomReal, 0.5) then
          begin
            alphaC.x := 2 * RandomReal - 1;
            alphaC.y := 2 * RandomReal - 1;
          end
        else
          begin
            alphaC := C_Complex(0);
          end;
        if AP_FP_Greater(RandomReal, 0.5) then
          begin
            betaC.x := 2 * RandomReal - 1;
            betaC.y := 2 * RandomReal - 1;
          end
        else
          begin
            betaC := C_Complex(0);
          end;

        //
        // copy C
        //
        i := 0;
        while i <= MaxN do
          begin
            j := 0;
            while j <= MaxN do
              begin
                RC1[i, j] := RefRC[i, j];
                RC2[i, j] := RefRC[i, j];
                CC1[i, j] := RefCC[i, j];
                CC2[i, j] := RefCC[i, j];
                inc(j);
              end;
            inc(i);
          end;

        //
        // Test TLComplex
        //
        CMatrixGEMM(M, n, k, alphaC, RefCA, AOffsI, AOffsJ, AOpType, RefCB, BOffsI, BOffsJ, BOpType, betaC, CC1, COffsI, COffsJ);
        RefCMatrixGEMM(M, n, k, alphaC, RefCA, AOffsI, AOffsJ, AOpType, RefCB, BOffsI, BOffsJ, BOpType, betaC, CC2, COffsI, COffsJ);
        i := 0;
        while i <= MaxN do
          begin
            j := 0;
            while j <= MaxN do
              begin
                Result := Result or AP_FP_Greater(AbsComplex(C_Sub(CC1[i, j], CC2[i, j])), Threshold);
                inc(j);
              end;
            inc(i);
          end;

        //
        // Test real
        //
        RMatrixGEMM(M, n, k, AlphaR, RefRA, AOffsI, AOffsJ, AOpTypeR, RefRB, BOffsI, BOffsJ, BOpTypeR, BetaR, RC1, COffsI, COffsJ);
        RefRMatrixGEMM(M, n, k, AlphaR, RefRA, AOffsI, AOffsJ, AOpTypeR, RefRB, BOffsI, BOffsJ, BOpTypeR, BetaR, RC2, COffsI, COffsJ);
        i := 0;
        while i <= MaxN do
          begin
            j := 0;
            while j <= MaxN do
              begin
                Result := Result or AP_FP_Greater(AbsReal(RC1[i, j] - RC2[i, j]), Threshold);
                inc(j);
              end;
            inc(i);
          end;
        inc(mx);
      end;
  end;

  function TestTrans(MinN: TLInt; MaxN: TLInt): Boolean;
  var
    M: TLInt;
    n: TLInt;
    mx: TLInt;
    i: TLInt;
    j: TLInt;
    AOffsI: TLInt;
    AOffsJ: TLInt;
    BOffsI: TLInt;
    BOffsJ: TLInt;
    v1: TLFloat;
    v2: TLFloat;
    Threshold: TLFloat;
    RefRA: TLMatrix;
    RefRB: TLMatrix;
    RefCA: TLComplexMatrix;
    RefCB: TLComplexMatrix;
  begin
    Result := False;
    Threshold := 1000 * MachineEpsilon;
    mx := MinN;
    while mx <= MaxN do
      begin

        //
        // Select random M/N in [1,MX] such that max(M,N)=MX
        // Generate random V1 and V2 which are used to fill
        // RefRB/RefCB with control values.
        //
        M := 1 + RandomInteger(mx);
        n := 1 + RandomInteger(mx);
        if RandomInteger(2) = 0 then
          begin
            M := mx;
          end
        else
          begin
            n := mx;
          end;
        v1 := RandomReal;
        v2 := RandomReal;

        //
        // Initialize A by random matrix with size (MaxN+1)*(MaxN+1)
        // Fill B with control values
        //
        SetLength(RefRA, MaxN + 1, MaxN + 1);
        SetLength(RefRB, MaxN + 1, MaxN + 1);
        SetLength(RefCA, MaxN + 1, MaxN + 1);
        SetLength(RefCB, MaxN + 1, MaxN + 1);
        i := 0;
        while i <= MaxN do
          begin
            j := 0;
            while j <= MaxN do
              begin
                RefRA[i, j] := 2 * RandomReal - 1;
                RefCA[i, j].x := 2 * RandomReal - 1;
                RefCA[i, j].y := 2 * RandomReal - 1;
                RefRB[i, j] := i * v1 + j * v2;
                RefCB[i, j] := C_Complex(i * v1 + j * v2);
                inc(j);
              end;
            inc(i);
          end;

        //
        // test different offsets (zero or one)
        //
        // to avoid unnecessary slowdown we don't test ALL possible
        // combinations of operation types. We just generate one random
        // set of parameters and test it.
        //
        AOffsI := RandomInteger(2);
        AOffsJ := RandomInteger(2);
        BOffsI := RandomInteger(2);
        BOffsJ := RandomInteger(2);
        RMatrixTranspose(M, n, RefRA, AOffsI, AOffsJ, RefRB, BOffsI, BOffsJ);
        i := 0;
        while i <= MaxN do
          begin
            j := 0;
            while j <= MaxN do
              begin
                if (i < BOffsI) or (i >= BOffsI + n) or (j < BOffsJ) or (j >= BOffsJ + M)
                then
                  begin
                    Result := Result or AP_FP_Greater(AbsReal(RefRB[i, j] - (v1 * i + v2 * j)), Threshold);
                  end
                else
                  begin
                    Result := Result or AP_FP_Greater(AbsReal(RefRB[i, j] - RefRA[AOffsI + j - BOffsJ, AOffsJ + i - BOffsI]), Threshold);
                  end;
                inc(j);
              end;
            inc(i);
          end;
        CMatrixTranspose(M, n, RefCA, AOffsI, AOffsJ, RefCB, BOffsI, BOffsJ);
        i := 0;
        while i <= MaxN do
          begin
            j := 0;
            while j <= MaxN do
              begin
                if (i < BOffsI) or (i >= BOffsI + n) or (j < BOffsJ) or (j >= BOffsJ + M)
                then
                  begin
                    Result := Result or AP_FP_Greater(AbsComplex(C_SubR(RefCB[i, j], v1 * i + v2 * j)), Threshold);
                  end
                else
                  begin
                    Result := Result or AP_FP_Greater(AbsComplex(C_Sub(RefCB[i, j], RefCA[AOffsI + j - BOffsJ, AOffsJ + i - BOffsI])), Threshold);
                  end;
                inc(j);
              end;
            inc(i);
          end;
        inc(mx);
      end;
  end;

  function TestRANK1(MinN: TLInt; MaxN: TLInt): Boolean;
  var
    M: TLInt;
    n: TLInt;
    mx: TLInt;
    i: TLInt;
    j: TLInt;
    AOffsI: TLInt;
    AOffsJ: TLInt;
    UOffs: TLInt;
    VOffs: TLInt;
    Threshold: TLFloat;
    RefRA: TLMatrix;
    RefRB: TLMatrix;
    RefCA: TLComplexMatrix;
    RefCB: TLComplexMatrix;
    RU: TLVec;
    RV: TLVec;
    CU: TLComplexVec;
    cv: TLComplexVec;
  begin
    Result := False;
    Threshold := 1000 * MachineEpsilon;
    mx := MinN;
    while mx <= MaxN do
      begin

        //
        // Select random M/N in [1,MX] such that max(M,N)=MX
        //
        M := 1 + RandomInteger(mx);
        n := 1 + RandomInteger(mx);
        if RandomInteger(2) = 0 then
          begin
            M := mx;
          end
        else
          begin
            n := mx;
          end;

        //
        // Initialize A by random matrix with size (MaxN+1)*(MaxN+1)
        // Fill B with control values
        //
        SetLength(RefRA, MaxN + MaxN, MaxN + MaxN);
        SetLength(RefRB, MaxN + MaxN, MaxN + MaxN);
        SetLength(RefCA, MaxN + MaxN, MaxN + MaxN);
        SetLength(RefCB, MaxN + MaxN, MaxN + MaxN);
        i := 0;
        while i <= 2 * MaxN - 1 do
          begin
            j := 0;
            while j <= 2 * MaxN - 1 do
              begin
                RefRA[i, j] := 2 * RandomReal - 1;
                RefCA[i, j].x := 2 * RandomReal - 1;
                RefCA[i, j].y := 2 * RandomReal - 1;
                RefRB[i, j] := RefRA[i, j];
                RefCB[i, j] := RefCA[i, j];
                inc(j);
              end;
            inc(i);
          end;
        SetLength(RU, 2 * M);
        SetLength(CU, 2 * M);
        i := 0;
        while i <= 2 * M - 1 do
          begin
            RU[i] := 2 * RandomReal - 1;
            CU[i].x := 2 * RandomReal - 1;
            CU[i].y := 2 * RandomReal - 1;
            inc(i);
          end;
        SetLength(RV, 2 * n);
        SetLength(cv, 2 * n);
        i := 0;
        while i <= 2 * n - 1 do
          begin
            RV[i] := 2 * RandomReal - 1;
            cv[i].x := 2 * RandomReal - 1;
            cv[i].y := 2 * RandomReal - 1;
            inc(i);
          end;

        //
        // test different offsets (zero or one)
        //
        // to avoid unnecessary slowdown we don't test ALL possible
        // combinations of operation types. We just generate one random
        // set of parameters and test it.
        //
        AOffsI := RandomInteger(MaxN);
        AOffsJ := RandomInteger(MaxN);
        UOffs := RandomInteger(M);
        VOffs := RandomInteger(n);
        CMatrixRank1(M, n, RefCA, AOffsI, AOffsJ, CU, UOffs, cv, VOffs);
        i := 0;
        while i <= 2 * MaxN - 1 do
          begin
            j := 0;
            while j <= 2 * MaxN - 1 do
              begin
                if (i < AOffsI) or (i >= AOffsI + M) or (j < AOffsJ) or (j >= AOffsJ + n)
                then
                  begin
                    Result := Result or AP_FP_Greater(AbsComplex(C_Sub(RefCA[i, j], RefCB[i, j])), Threshold);
                  end
                else
                  begin
                    Result := Result or AP_FP_Greater(AbsComplex(C_Sub(RefCA[i, j], C_Add(RefCB[i, j], C_Mul(CU[i - AOffsI + UOffs], cv[j - AOffsJ + VOffs])))), Threshold);
                  end;
                inc(j);
              end;
            inc(i);
          end;
        RMatrixRank1(M, n, RefRA, AOffsI, AOffsJ, RU, UOffs, RV, VOffs);
        i := 0;
        while i <= 2 * MaxN - 1 do
          begin
            j := 0;
            while j <= 2 * MaxN - 1 do
              begin
                if (i < AOffsI) or (i >= AOffsI + M) or (j < AOffsJ) or (j >= AOffsJ + n)
                then
                  begin
                    Result := Result or AP_FP_Greater(AbsReal(RefRA[i, j] - RefRB[i, j]), Threshold);
                  end
                else
                  begin
                    Result := Result or AP_FP_Greater(AbsReal(RefRA[i, j] - (RefRB[i, j] + RU[i - AOffsI + UOffs] * RV[j - AOffsJ + VOffs])), Threshold);
                  end;
                inc(j);
              end;
            inc(i);
          end;
        inc(mx);
      end;
  end;

  function TestMV(MinN: TLInt; MaxN: TLInt): Boolean;
  var
    M: TLInt;
    n: TLInt;
    mx: TLInt;
    i: TLInt;
    j: TLInt;
    AOffsI: TLInt;
    AOffsJ: TLInt;
    XOffs: TLInt;
    YOffs: TLInt;
    OpCA: TLInt;
    OpRA: TLInt;
    Threshold: TLFloat;
    RV1: TLFloat;
    RV2: TLFloat;
    CV1: TLComplex;
    CV2: TLComplex;
    RefRA: TLMatrix;
    RefCA: TLComplexMatrix;
    RX: TLVec;
    RY: TLVec;
    Cx: TLComplexVec;
    Cy: TLComplexVec;
    i_: TLInt;
    i1_: TLInt;
  begin
    Result := False;
    Threshold := 1000 * MachineEpsilon;
    mx := MinN;
    while mx <= MaxN do
      begin

        //
        // Select random M/N in [1,MX] such that max(M,N)=MX
        //
        M := 1 + RandomInteger(mx);
        n := 1 + RandomInteger(mx);
        if RandomInteger(2) = 0 then
          begin
            M := mx;
          end
        else
          begin
            n := mx;
          end;

        //
        // Initialize A by random matrix with size (MaxN+MaxN)*(MaxN+MaxN)
        // Initialize X by random vector with size (MaxN+MaxN)
        // Fill Y by control values
        //
        SetLength(RefRA, MaxN + MaxN, MaxN + MaxN);
        SetLength(RefCA, MaxN + MaxN, MaxN + MaxN);
        i := 0;
        while i <= 2 * MaxN - 1 do
          begin
            j := 0;
            while j <= 2 * MaxN - 1 do
              begin
                RefRA[i, j] := 2 * RandomReal - 1;
                RefCA[i, j].x := 2 * RandomReal - 1;
                RefCA[i, j].y := 2 * RandomReal - 1;
                inc(j);
              end;
            inc(i);
          end;
        SetLength(RX, 2 * MaxN);
        SetLength(Cx, 2 * MaxN);
        SetLength(RY, 2 * MaxN);
        SetLength(Cy, 2 * MaxN);
        i := 0;
        while i <= 2 * MaxN - 1 do
          begin
            RX[i] := 2 * RandomReal - 1;
            Cx[i].x := 2 * RandomReal - 1;
            Cx[i].y := 2 * RandomReal - 1;
            RY[i] := i;
            Cy[i] := C_Complex(i);
            inc(i);
          end;

        //
        // test different offsets (zero or one)
        //
        // to avoid unnecessary slowdown we don't test ALL possible
        // combinations of operation types. We just generate one random
        // set of parameters and test it.
        //
        AOffsI := RandomInteger(MaxN);
        AOffsJ := RandomInteger(MaxN);
        XOffs := RandomInteger(MaxN);
        YOffs := RandomInteger(MaxN);
        OpCA := RandomInteger(3);
        OpRA := RandomInteger(2);
        CMatrixMV(M, n, RefCA, AOffsI, AOffsJ, OpCA, Cx, XOffs, Cy, YOffs);
        i := 0;
        while i <= 2 * MaxN - 1 do
          begin
            if (i < YOffs) or (i >= YOffs + M) then
              begin
                Result := Result or C_NotEqualR(Cy[i], i);
              end
            else
              begin
                CV1 := Cy[i];
                if OpCA = 0 then
                  begin
                    i1_ := (XOffs) - (AOffsJ);
                    CV2 := C_Complex(0.0);
                    for i_ := AOffsJ to AOffsJ + n - 1 do
                      begin
                        CV2 := C_Add(CV2, C_Mul(RefCA[AOffsI + i - YOffs, i_], Cx[i_ + i1_]));
                      end;
                  end;
                if OpCA = 1 then
                  begin
                    i1_ := (XOffs) - (AOffsI);
                    CV2 := C_Complex(0.0);
                    for i_ := AOffsI to AOffsI + n - 1 do
                      begin
                        CV2 := C_Add(CV2, C_Mul(RefCA[i_, AOffsJ + i - YOffs], Cx[i_ + i1_]));
                      end;
                  end;
                if OpCA = 2 then
                  begin
                    i1_ := (XOffs) - (AOffsI);
                    CV2 := C_Complex(0.0);
                    for i_ := AOffsI to AOffsI + n - 1 do
                      begin
                        CV2 := C_Add(CV2, C_Mul(Conj(RefCA[i_, AOffsJ + i - YOffs]), Cx[i_ + i1_]));
                      end;
                  end;
                Result := Result or AP_FP_Greater(AbsComplex(C_Sub(CV1, CV2)), Threshold);
              end;
            inc(i);
          end;
        RMatrixMV(M, n, RefRA, AOffsI, AOffsJ, OpRA, RX, XOffs, RY, YOffs);
        i := 0;
        while i <= 2 * MaxN - 1 do
          begin
            if (i < YOffs) or (i >= YOffs + M) then
              begin
                Result := Result or AP_FP_NEq(RY[i], i);
              end
            else
              begin
                RV1 := RY[i];
                if OpRA = 0 then
                  begin
                    RV2 := APVDotProduct(@RefRA[AOffsI + i - YOffs][0], AOffsJ, AOffsJ + n - 1, @RX[0], XOffs, XOffs + n - 1);
                  end;
                if OpRA = 1 then
                  begin
                    i1_ := (XOffs) - (AOffsI);
                    RV2 := 0.0;
                    for i_ := AOffsI to AOffsI + n - 1 do
                      begin
                        RV2 := RV2 + RefRA[i_, AOffsJ + i - YOffs] * RX[i_ + i1_];
                      end;
                  end;
                Result := Result or AP_FP_Greater(AbsReal(RV1 - RV2), Threshold);
              end;
            inc(i);
          end;
        inc(mx);
      end;
  end;

  function TestCopy(MinN: TLInt; MaxN: TLInt): Boolean;
  var
    M: TLInt;
    n: TLInt;
    mx: TLInt;
    i: TLInt;
    j: TLInt;
    AOffsI: TLInt;
    AOffsJ: TLInt;
    BOffsI: TLInt;
    BOffsJ: TLInt;
    Threshold: TLFloat;
    RV1: TLFloat;
    RV2: TLFloat;
    CV1: TLComplex;
    CV2: TLComplex;
    ResArry: TLMatrix;
    rb: TLMatrix;
    ca: TLComplexMatrix;
    CB: TLComplexMatrix;
  begin
    Result := False;
    Threshold := 1000 * MachineEpsilon;
    mx := MinN;
    while mx <= MaxN do
      begin

        //
        // Select random M/N in [1,MX] such that max(M,N)=MX
        //
        M := 1 + RandomInteger(mx);
        n := 1 + RandomInteger(mx);
        if RandomInteger(2) = 0 then
          begin
            M := mx;
          end
        else
          begin
            n := mx;
          end;

        //
        // Initialize A by random matrix with size (MaxN+MaxN)*(MaxN+MaxN)
        // Initialize X by random vector with size (MaxN+MaxN)
        // Fill Y by control values
        //
        SetLength(ResArry, MaxN + MaxN, MaxN + MaxN);
        SetLength(ca, MaxN + MaxN, MaxN + MaxN);
        SetLength(rb, MaxN + MaxN, MaxN + MaxN);
        SetLength(CB, MaxN + MaxN, MaxN + MaxN);
        i := 0;
        while i <= 2 * MaxN - 1 do
          begin
            j := 0;
            while j <= 2 * MaxN - 1 do
              begin
                ResArry[i, j] := 2 * RandomReal - 1;
                ca[i, j].x := 2 * RandomReal - 1;
                ca[i, j].y := 2 * RandomReal - 1;
                rb[i, j] := 1 + 2 * i + 3 * j;
                CB[i, j] := C_Complex(1 + 2 * i + 3 * j);
                inc(j);
              end;
            inc(i);
          end;

        //
        // test different offsets (zero or one)
        //
        // to avoid unnecessary slowdown we don't test ALL possible
        // combinations of operation types. We just generate one random
        // set of parameters and test it.
        //
        AOffsI := RandomInteger(MaxN);
        AOffsJ := RandomInteger(MaxN);
        BOffsI := RandomInteger(MaxN);
        BOffsJ := RandomInteger(MaxN);
        CMatrixCopy(M, n, ca, AOffsI, AOffsJ, CB, BOffsI, BOffsJ);
        i := 0;
        while i <= 2 * MaxN - 1 do
          begin
            j := 0;
            while j <= 2 * MaxN - 1 do
              begin
                if (i < BOffsI) or (i >= BOffsI + M) or (j < BOffsJ) or (j >= BOffsJ + n)
                then
                  begin
                    Result := Result or C_NotEqualR(CB[i, j], 1 + 2 * i + 3 * j);
                  end
                else
                  begin
                    Result := Result or AP_FP_Greater(AbsComplex(C_Sub(ca[AOffsI + i - BOffsI, AOffsJ + j - BOffsJ], CB[i, j])), Threshold);
                  end;
                inc(j);
              end;
            inc(i);
          end;
        RMatrixCopy(M, n, ResArry, AOffsI, AOffsJ, rb, BOffsI, BOffsJ);
        i := 0;
        while i <= 2 * MaxN - 1 do
          begin
            j := 0;
            while j <= 2 * MaxN - 1 do
              begin
                if (i < BOffsI) or (i >= BOffsI + M) or (j < BOffsJ) or (j >= BOffsJ + n)
                then
                  begin
                    Result := Result or AP_FP_NEq(rb[i, j], 1 + 2 * i + 3 * j);
                  end
                else
                  begin
                    Result := Result or AP_FP_Greater(AbsReal(ResArry[AOffsI + i - BOffsI, AOffsJ + j - BOffsJ] - rb[i, j]), Threshold);
                  end;
                inc(j);
              end;
            inc(i);
          end;
        inc(mx);
      end;
  end;

var
  Threshold: TLFloat;
  TRSMErrors: Boolean;
  SYRKErrors: Boolean;
  GEMMErrors: Boolean;
  TRANSErrors: Boolean;
  RANK1Errors: Boolean;
  MVErrors: Boolean;
  CopyErrors: Boolean;
  WasErrors: Boolean;
  ResArry: TLMatrix;
begin
  TRSMErrors := False;
  SYRKErrors := False;
  GEMMErrors := False;
  TRANSErrors := False;
  RANK1Errors := False;
  MVErrors := False;
  CopyErrors := False;
  WasErrors := False;
  Threshold := 10000 * MachineEpsilon;
  DoStatus('Check ABLAS Memory.', []);
  TRSMErrors := TRSMErrors or TestTRSM(1, 3 * ABLASBlockSize(ResArry) + 1);
  SYRKErrors := SYRKErrors or TestSYRK(1, 3 * ABLASBlockSize(ResArry) + 1);
  GEMMErrors := GEMMErrors or TestGEMM(1, 3 * ABLASBlockSize(ResArry) + 1);
  TRANSErrors := TRANSErrors or TestTrans(1, 3 * ABLASBlockSize(ResArry) + 1);
  RANK1Errors := RANK1Errors or TestRANK1(1, 3 * ABLASBlockSize(ResArry) + 1);
  MVErrors := MVErrors or TestMV(1, 3 * ABLASBlockSize(ResArry) + 1);
  CopyErrors := CopyErrors or TestCopy(1, 3 * ABLASBlockSize(ResArry) + 1);

  //
  // report
  //
  WasErrors := TRSMErrors or SYRKErrors or GEMMErrors or TRANSErrors or
    RANK1Errors or MVErrors or CopyErrors;
  if not Silent then
    begin
      DoStatusNoLn(PFormat('TESTING ABLAS'#13#10'', []));
      DoStatusNoLn(PFormat('* TRSM:                                  ', []));
      if TRSMErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* SYRK:                                  ', []));
      if SYRKErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* GEMM:                                  ', []));
      if GEMMErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* TRANS:                                 ', []));
      if TRANSErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* RANK1:                                 ', []));
      if RANK1Errors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* MV:                                    ', []));
      if MVErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* COPY:                                  ', []));
      if CopyErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      if WasErrors then
        begin
          DoStatusNoLn(PFormat('TEST FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('TEST PASSED'#13#10'', []));
        end;
      DoStatusNoLn(PFormat(''#13#10''#13#10'', []));
    end;
  Result := not WasErrors;
end;

function TestXBLAS(Silent: Boolean): Boolean;
var
  ApproxErrors: Boolean;
  ExactnessErrors: Boolean;
  WasErrors: Boolean;
  ApproxThreshold: TLFloat;
  MaxN: TLInt;
  PassCount: TLInt;
  n: TLInt;
  i: TLInt;
  pass: TLInt;
  RV1: TLFloat;
  RV2: TLFloat;
  RV2Err: TLFloat;
  CV1: TLComplex;
  CV2: TLComplex;
  CV2Err: TLFloat;
  CV2ErrX: TLFloat;
  CV2ErrY: TLFloat;
  RX: TLVec;
  RY: TLVec;
  Cx: TLComplexVec;
  Cy: TLComplexVec;
  Temp: TLVec;
  b: TLFloat;
  s: TLFloat;
  i_: TLInt;
begin
  ApproxErrors := False;
  ExactnessErrors := False;
  WasErrors := False;
  ApproxThreshold := 1000 * MachineEpsilon;
  MaxN := 1000;
  PassCount := 10;

  DoStatus('Check XBLAS dot product.', []);
  //
  // tests:
  // 1. ability to calculate dot product
  // 2. higher precision
  //
  n := 1;
  while n <= MaxN do
    begin
      pass := 1;
      while pass <= PassCount do
        begin

          //
          // ability to approximately calculate real dot product
          //
          SetLength(RX, n);
          SetLength(RY, n);
          SetLength(Temp, n);
          i := 0;
          while i <= n - 1 do
            begin
              if AP_FP_Greater(RandomReal, 0.2) then
                begin
                  RX[i] := 2 * RandomReal - 1;
                end
              else
                begin
                  RX[i] := 0;
                end;
              if AP_FP_Greater(RandomReal, 0.2) then
                begin
                  RY[i] := 2 * RandomReal - 1;
                end
              else
                begin
                  RY[i] := 0;
                end;
              inc(i);
            end;
          RV1 := APVDotProduct(@RX[0], 0, n - 1, @RY[0], 0, n - 1);
          XDot(RX, RY, n, Temp, RV2, RV2Err);
          ApproxErrors := ApproxErrors or AP_FP_Greater(AbsReal(RV1 - RV2), ApproxThreshold);

          //
          // ability to approximately calculate TLComplex dot product
          //
          SetLength(Cx, n);
          SetLength(Cy, n);
          SetLength(Temp, 2 * n);
          i := 0;
          while i <= n - 1 do
            begin
              if AP_FP_Greater(RandomReal, 0.2) then
                begin
                  Cx[i].x := 2 * RandomReal - 1;
                  Cx[i].y := 2 * RandomReal - 1;
                end
              else
                begin
                  Cx[i] := C_Complex(0);
                end;
              if AP_FP_Greater(RandomReal, 0.2) then
                begin
                  Cy[i].x := 2 * RandomReal - 1;
                  Cy[i].y := 2 * RandomReal - 1;
                end
              else
                begin
                  Cy[i] := C_Complex(0);
                end;
              inc(i);
            end;
          CV1 := C_Complex(0.0);
          for i_ := 0 to n - 1 do
            begin
              CV1 := C_Add(CV1, C_Mul(Cx[i_], Cy[i_]));
            end;
          XCDot(Cx, Cy, n, Temp, CV2, CV2Err);
          ApproxErrors := ApproxErrors or AP_FP_Greater(AbsComplex(C_Sub(CV1, CV2)), ApproxThreshold);
          inc(pass);
        end;
      inc(n);
    end;

  DoStatus('Check XBLAS real precision.', []);
  //
  // test of precision: real
  //
  n := 50000;
  SetLength(RX, n);
  SetLength(RY, n);
  SetLength(Temp, n);
  pass := 0;
  while pass <= PassCount - 1 do
    begin
      Assert(n mod 2 = 0);

      //
      // First test: X + X + ... + X - X - X - ... - X = 1*X
      //
      s := Exp(Max(pass, 50));
      if (pass = PassCount - 1) and (pass > 1) then
        begin
          s := MaxRealNumber;
        end;
      RY[0] := (2 * RandomReal - 1) * s * Sqrt(2 * RandomReal);
      i := 1;
      while i <= n - 1 do
        begin
          RY[i] := RY[0];
          inc(i);
        end;
      i := 0;
      while i <= n div 2 - 1 do
        begin
          RX[i] := 1;
          inc(i);
        end;
      i := n div 2;
      while i <= n - 2 do
        begin
          RX[i] := -1;
          inc(i);
        end;
      RX[n - 1] := 0;
      XDot(RX, RY, n, Temp, RV2, RV2Err);
      ExactnessErrors := ExactnessErrors or AP_FP_Less(RV2Err, 0);
      ExactnessErrors := ExactnessErrors or AP_FP_Greater(RV2Err, 4 * MachineEpsilon * AbsReal(RY[0]));
      ExactnessErrors := ExactnessErrors or AP_FP_Greater(AbsReal(RV2 - RY[0]), RV2Err);

      //
      // First test: X + X + ... + X = N*X
      //
      s := Exp(Max(pass, 50));
      if (pass = PassCount - 1) and (pass > 1) then
        begin
          s := MaxRealNumber;
        end;
      RY[0] := (2 * RandomReal - 1) * s * Sqrt(2 * RandomReal);
      i := 1;
      while i <= n - 1 do
        begin
          RY[i] := RY[0];
          inc(i);
        end;
      i := 0;
      while i <= n - 1 do
        begin
          RX[i] := 1;
          inc(i);
        end;
      XDot(RX, RY, n, Temp, RV2, RV2Err);
      ExactnessErrors := ExactnessErrors or AP_FP_Less(RV2Err, 0);
      ExactnessErrors := ExactnessErrors or AP_FP_Greater(RV2Err, 4 * MachineEpsilon * AbsReal(RY[0]) * n);
      ExactnessErrors := ExactnessErrors or AP_FP_Greater(AbsReal(RV2 - n * RY[0]), RV2Err);
      inc(pass);
    end;

  DoStatus('Check XBLAS TLComplex.', []);
  //
  // test of precision: TLComplex
  //
  n := 50000;
  SetLength(Cx, n);
  SetLength(Cy, n);
  SetLength(Temp, 2 * n);
  pass := 0;
  while pass <= PassCount - 1 do
    begin
      Assert(n mod 2 = 0);

      //
      // First test: X + X + ... + X - X - X - ... - X = 1*X
      //
      s := Exp(Max(pass, 50));
      if (pass = PassCount - 1) and (pass > 1) then
        begin
          s := MaxRealNumber;
        end;
      Cy[0].x := (2 * RandomReal - 1) * s * Sqrt(2 * RandomReal);
      Cy[0].y := (2 * RandomReal - 1) * s * Sqrt(2 * RandomReal);
      i := 1;
      while i <= n - 1 do
        begin
          Cy[i] := Cy[0];
          inc(i);
        end;
      i := 0;
      while i <= n div 2 - 1 do
        begin
          Cx[i] := C_Complex(1);
          inc(i);
        end;
      i := n div 2;
      while i <= n - 2 do
        begin
          Cx[i] := C_Complex(-1);
          inc(i);
        end;
      Cx[n - 1] := C_Complex(0);
      XCDot(Cx, Cy, n, Temp, CV2, CV2Err);
      ExactnessErrors := ExactnessErrors or AP_FP_Less(CV2Err, 0);
      ExactnessErrors := ExactnessErrors or AP_FP_Greater(CV2Err, 4 * MachineEpsilon * AbsComplex(Cy[0]));
      ExactnessErrors := ExactnessErrors or AP_FP_Greater(AbsComplex(C_Sub(CV2, Cy[0])), CV2Err);

      //
      // First test: X + X + ... + X = N*X
      //
      s := Exp(Max(pass, 50));
      if (pass = PassCount - 1) and (pass > 1) then
        begin
          s := MaxRealNumber;
        end;
      Cy[0] := C_Complex((2 * RandomReal - 1) * s * Sqrt(2 * RandomReal));
      i := 1;
      while i <= n - 1 do
        begin
          Cy[i] := Cy[0];
          inc(i);
        end;
      i := 0;
      while i <= n - 1 do
        begin
          Cx[i] := C_Complex(1);
          inc(i);
        end;
      XCDot(Cx, Cy, n, Temp, CV2, CV2Err);
      ExactnessErrors := ExactnessErrors or AP_FP_Less(CV2Err, 0);
      ExactnessErrors := ExactnessErrors or AP_FP_Greater(CV2Err, 4 * MachineEpsilon * AbsComplex(Cy[0]) * n);
      ExactnessErrors := ExactnessErrors or AP_FP_Greater(AbsComplex(C_Sub(CV2, C_MulR(Cy[0], n))), CV2Err);
      inc(pass);
    end;

  //
  // report
  //
  WasErrors := ApproxErrors or ExactnessErrors;
  if not Silent then
    begin
      DoStatusNoLn(PFormat('TESTING XBLAS'#13#10'', []));
      DoStatusNoLn(PFormat('APPROX.TESTS:                            ', []));
      if ApproxErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('EXACT TESTS:                             ', []));
      if ExactnessErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      if WasErrors then
        begin
          DoStatusNoLn(PFormat('TEST FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('TEST PASSED'#13#10'', []));
        end;
      DoStatusNoLn(PFormat(''#13#10''#13#10'', []));
    end;

  //
  // end
  //
  Result := not WasErrors;
end;

function TestGaussKronrodQuadrature(Silent: Boolean): Boolean;
(* ************************************************************************
  Maps:
  0   =>  -0.9
  1   =>  -0.5
  2   =>  -0.1
  3   =>   0.0
  4   =>  +0.1
  5   =>  +0.5
  6   =>  +0.9
  7   =>  +1.0
  8   =>  +1.5
  9   =>  +2.0
  ************************************************************************ *)
  function MapKind(k: TLInt): TLFloat;
  begin
    Result := 0;
    if k = 0 then
      begin
        Result := -0.9;
      end;
    if k = 1 then
      begin
        Result := -0.5;
      end;
    if k = 2 then
      begin
        Result := -0.1;
      end;
    if k = 3 then
      begin
        Result := 0.0;
      end;
    if k = 4 then
      begin
        Result := +0.1;
      end;
    if k = 5 then
      begin
        Result := +0.5;
      end;
    if k = 6 then
      begin
        Result := +0.9;
      end;
    if k = 7 then
      begin
        Result := +1.0;
      end;
    if k = 8 then
      begin
        Result := +1.5;
      end;
    if k = 9 then
      begin
        Result := +2.0;
      end;
  end;

var
  PKind: TLInt;
  ErrTol: TLFloat;
  Eps: TLFloat;
  NonStrictErrTol: TLFloat;
  n: TLInt;
  i: TLInt;
  k: TLInt;
  Info: TLInt;
  Err: TLFloat;
  AKind: TLInt;
  BKind: TLInt;
  alphaC: TLFloat;
  betaC: TLFloat;
  x1: TLVec;
  WG1: TLVec;
  WK1: TLVec;
  x2: TLVec;
  WG2: TLVec;
  WK2: TLVec;
  Info1: TLInt;
  Info2: TLInt;
  SuccessAtLeastOnce: Boolean;
  InTblErrors: Boolean;
  VsTblErrors: Boolean;
  GenErrors: Boolean;
  WasErrors: Boolean;
begin
  InTblErrors := False;
  VsTblErrors := False;
  GenErrors := False;
  WasErrors := False;
  ErrTol := 10000 * MachineEpsilon;
  NonStrictErrTol := 1000 * ErrTol;

  //
  // test recurrence-based Legendre nodes against the precalculated table
  //
  PKind := 0;
  while PKind <= 5 do
    begin
      n := 0;
      if PKind = 0 then
        begin
          n := 15;
        end;
      if PKind = 1 then
        begin
          n := 21;
        end;
      if PKind = 2 then
        begin
          n := 31;
        end;
      if PKind = 3 then
        begin
          n := 41;
        end;
      if PKind = 4 then
        begin
          n := 51;
        end;
      if PKind = 5 then
        begin
          n := 61;
        end;
      GaussKronrodQuadratureLegendreCalc(n, Info, x1, WK1, WG1);
      GaussKronrodQuadratureLegendreTbl(n, x2, WK2, WG2, Eps);
      if Info <= 0 then
        begin
          GenErrors := True;
          Break;
        end;
      i := 0;
      while i <= n - 1 do
        begin
          VsTblErrors := VsTblErrors or
            AP_FP_Greater(AbsReal(x1[i] - x2[i]), ErrTol);
          VsTblErrors := VsTblErrors or
            AP_FP_Greater(AbsReal(WK1[i] - WK2[i]), ErrTol);
          VsTblErrors := VsTblErrors or
            AP_FP_Greater(AbsReal(WG1[i] - WG2[i]), ErrTol);
          inc(i);
        end;
      inc(PKind);
    end;

  //
  // Test recurrence-baced Gauss-Kronrod nodes against Gauss-only nodes
  // calculated with subroutines from GaussQuadrature unit.
  //
  k := 1;
  while k <= 30 do
    begin
      n := 2 * k + 1;

      //
      // Gauss-Legendre
      //
      Err := 0;
      GaussKronrodQuadratureGenerateGaussLegendre(n, Info1, x1, WK1, WG1);
      GaussQuadratureGenerateGaussLegendre(k, Info2, x2, WG2);
      if (Info1 > 0) and (Info2 > 0) then
        begin
          i := 0;
          while i <= k - 1 do
            begin
              Err := Max(Err, AbsReal(x1[2 * i + 1] - x2[i]));
              Err := Max(Err, AbsReal(WG1[2 * i + 1] - WG2[i]));
              inc(i);
            end;
        end
      else
        begin
          GenErrors := True;
        end;
      GenErrors := GenErrors or AP_FP_Greater(Err, ErrTol);
      inc(k);
    end;
  k := 1;
  while k <= 15 do
    begin
      n := 2 * k + 1;

      //
      // Gauss-Jacobi
      //
      SuccessAtLeastOnce := False;
      Err := 0;
      AKind := 0;
      while AKind <= 9 do
        begin
          BKind := 0;
          while BKind <= 9 do
            begin
              alphaC := MapKind(AKind);
              betaC := MapKind(BKind);
              GaussKronrodQuadratureGenerateGaussJacobi(n, alphaC, betaC, Info1, x1, WK1, WG1);
              GaussQuadratureGenerateGaussJacobi(k, alphaC, betaC, Info2, x2, WG2);
              if (Info1 > 0) and (Info2 > 0) then
                begin
                  SuccessAtLeastOnce := True;
                  i := 0;
                  while i <= k - 1 do
                    begin
                      Err := Max(Err, AbsReal(x1[2 * i + 1] - x2[i]));
                      Err := Max(Err, AbsReal(WG1[2 * i + 1] - WG2[i]));
                      inc(i);
                    end;
                end
              else
                begin
                  GenErrors := GenErrors or (Info1 <> -5);
                end;
              inc(BKind);
            end;
          inc(AKind);
        end;
      GenErrors := GenErrors or AP_FP_Greater(Err, ErrTol) or
        not SuccessAtLeastOnce;
      inc(k);
    end;

  //
  // end
  //
  WasErrors := InTblErrors or VsTblErrors or GenErrors;
  if not Silent then
    begin
      DoStatusNoLn(PFormat('TESTING GAUSS-KRONROD QUADRATURES'#13#10'', []));
      DoStatusNoLn(PFormat('FINAL RESULT:                             ', []));
      if WasErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* PRE-CALCULATED TABLE:                   ', []));
      if InTblErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* CALCULATED AGAINST THE TABLE:           ', []));
      if VsTblErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* GENERAL PROPERTIES:                     ', []));
      if GenErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      if WasErrors then
        begin
          DoStatusNoLn(PFormat('TEST FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('TEST PASSED'#13#10'', []));
        end;
      DoStatusNoLn(PFormat(''#13#10''#13#10'', []));
    end;
  Result := not WasErrors;
end;

(* ************************************************************************
  Gauss-Legendre, another variant
  ************************************************************************ *)
procedure BuildGaussLegendreQuadrature(n: TLInt; var x: TLVec; var w: TLVec);
var
  i: TLInt;
  j: TLInt;
  r: TLFloat;
  r1: TLFloat;
  p1: TLFloat;
  p2: TLFloat;
  p3: TLFloat;
  dp3: TLFloat;
  tmp: TLFloat;
begin
  SetLength(x, n);
  SetLength(w, n);
  i := 0;
  while i <= (n + 1) div 2 - 1 do
    begin
      r := Cos(pi * (4 * i + 3) / (4 * n + 2));
      repeat
        p2 := 0;
        p3 := 1;
        j := 0;
        while j <= n - 1 do
          begin
            p1 := p2;
            p2 := p3;
            p3 := ((2 * j + 1) * r * p2 - j * p1) / (j + 1);
            inc(j);
          end;
        dp3 := n * (r * p3 - p2) / (r * r - 1);
        r1 := r;
        r := r - p3 / dp3;
      until AP_FP_Less(AbsReal(r - r1), MachineEpsilon * (1 + AbsReal(r)) * 100);
      x[i] := r;
      x[n - 1 - i] := -r;
      w[i] := 2 / ((1 - r * r) * dp3 * dp3);
      w[n - 1 - i] := 2 / ((1 - r * r) * dp3 * dp3);
      inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 2 - i do
        begin
          if AP_FP_Greater_Eq(x[j], x[j + 1]) then
            begin
              tmp := x[j];
              x[j] := x[j + 1];
              x[j + 1] := tmp;
              tmp := w[j];
              w[j] := w[j + 1];
              w[j + 1] := tmp;
            end;
          inc(j);
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Gauss-Jacobi, another variant
  ************************************************************************ *)
procedure BuildGaussJacobiQuadrature(n: TLInt; alpha: TLFloat; beta: TLFloat; var x: TLVec; var w: TLVec);
var
  i: TLInt;
  j: TLInt;
  r: TLFloat;
  r1: TLFloat;
  t1: TLFloat;
  t2: TLFloat;
  t3: TLFloat;
  p1: TLFloat;
  p2: TLFloat;
  p3: TLFloat;
  PP: TLFloat;
  AN: TLFloat;
  BN: TLFloat;
  a: TLFloat;
  b: TLFloat;
  c: TLFloat;
  tmpsgn: TLFloat;
  tmp: TLFloat;
  alfbet: TLFloat;
  Temp: TLFloat;
  ITS: TLInt;
begin
  SetLength(x, n);
  SetLength(w, n);
  i := 0;
  while i <= n - 1 do
    begin
      if i = 0 then
        begin
          AN := alpha / n;
          BN := beta / n;
          t1 := (1 + alpha) * (2.78 / (4 + n * n) + 0.768 * AN / n);
          t2 := 1 + 1.48 * AN + 0.96 * BN + 0.452 * AN * AN + 0.83 * AN * BN;
          r := (t2 - t1) / t2;
        end
      else
        begin
          if i = 1 then
            begin
              t1 := (4.1 + alpha) / ((1 + alpha) * (1 + 0.156 * alpha));
              t2 := 1 + 0.06 * (n - 8) * (1 + 0.12 * alpha) / n;
              t3 := 1 + 0.012 * beta * (1 + 0.25 * AbsReal(alpha)) / n;
              r := r - t1 * t2 * t3 * (1 - r);
            end
          else
            begin
              if i = 2 then
                begin
                  t1 := (1.67 + 0.28 * alpha) / (1 + 0.37 * alpha);
                  t2 := 1 + 0.22 * (n - 8) / n;
                  t3 := 1 + 8 * beta / ((6.28 + beta) * n * n);
                  r := r - t1 * t2 * t3 * (x[0] - r);
                end
              else
                begin
                  if i < n - 2 then
                    begin
                      r := 3 * x[i - 1] - 3 * x[i - 2] + x[i - 3];
                    end
                  else
                    begin
                      if i = n - 2 then
                        begin
                          t1 := (1 + 0.235 * beta) / (0.766 + 0.119 * beta);
                          t2 := 1 / (1 + 0.639 * (n - 4) / (1 + 0.71 * (n - 4)));
                          t3 := 1 / (1 + 20 * alpha / ((7.5 + alpha) * n * n));
                          r := r + t1 * t2 * t3 * (r - x[i - 2]);
                        end
                      else
                        begin
                          if i = n - 1 then
                            begin
                              t1 := (1 + 0.37 * beta) / (1.67 + 0.28 * beta);
                              t2 := 1 / (1 + 0.22 * (n - 8) / n);
                              t3 := 1 / (1 + 8 * alpha / ((6.28 + alpha) * n * n));
                              r := r + t1 * t2 * t3 * (r - x[i - 2]);
                            end;
                        end;
                    end;
                end;
            end;
        end;
      alfbet := alpha + beta;
      repeat
        Temp := 2 + alfbet;
        p1 := (alpha - beta + Temp * r) * 0.5;
        p2 := 1;
        j := 2;
        while j <= n do
          begin
            p3 := p2;
            p2 := p1;
            Temp := 2 * j + alfbet;
            a := 2 * j * (j + alfbet) * (Temp - 2);
            b := (Temp - 1) * (alpha * alpha - beta * beta + Temp * (Temp - 2) * r);
            c := 2 * (j - 1 + alpha) * (j - 1 + beta) * Temp;
            p1 := (b * p2 - c * p3) / a;
            inc(j);
          end;
        PP := (n * (alpha - beta - Temp * r) * p1 + 2 * (n + alpha) * (n + beta) *
          p2) / (Temp * (1 - r * r));
        r1 := r;
        r := r1 - p1 / PP;
      until AP_FP_Less(AbsReal(r - r1), MachineEpsilon * (1 + AbsReal(r)) * 100);
      x[i] := r;
      w[i] := Exp(LnGamma(alpha + n, tmpsgn) + LnGamma(beta + n, tmpsgn) -
        LnGamma(n + 1, tmpsgn) - LnGamma(n + alfbet + 1, tmpsgn)) * Temp *
        Power(2, alfbet) / (PP * p2);
      inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 2 - i do
        begin
          if AP_FP_Greater_Eq(x[j], x[j + 1]) then
            begin
              tmp := x[j];
              x[j] := x[j + 1];
              x[j + 1] := tmp;
              tmp := w[j];
              w[j] := w[j + 1];
              w[j + 1] := tmp;
            end;
          inc(j);
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Gauss-Laguerre, another variant
  ************************************************************************ *)
procedure BuildGaussLaguerreQuadrature(n: TLInt; alpha: TLFloat; var x: TLVec; var w: TLVec);
var
  i: TLInt;
  j: TLInt;
  r: TLFloat;
  r1: TLFloat;
  p1: TLFloat;
  p2: TLFloat;
  p3: TLFloat;
  dp3: TLFloat;
  tsg: TLFloat;
  tmp: TLFloat;
begin
  SetLength(x, n);
  SetLength(w, n);
  i := 0;
  while i <= n - 1 do
    begin
      if i = 0 then
        begin
          r := (1 + alpha) * (3 + 0.92 * alpha) / (1 + 2.4 * n + 1.8 * alpha);
        end
      else
        begin
          if i = 1 then
            begin
              r := r + (15 + 6.25 * alpha) / (1 + 0.9 * alpha + 2.5 * n);
            end
          else
            begin
              r := r + ((1 + 2.55 * (i - 1)) / (1.9 * (i - 1)) + 1.26 * (i - 1) *
                alpha / (1 + 3.5 * (i - 1))) / (1 + 0.3 * alpha) * (r - x[i - 2]);
            end;
        end;
      repeat
        p2 := 0;
        p3 := 1;
        j := 0;
        while j <= n - 1 do
          begin
            p1 := p2;
            p2 := p3;
            p3 := ((-r + 2 * j + alpha + 1) * p2 - (j + alpha) * p1) / (j + 1);
            inc(j);
          end;
        dp3 := (n * p3 - (n + alpha) * p2) / r;
        r1 := r;
        r := r - p3 / dp3;
      until AP_FP_Less(AbsReal(r - r1), MachineEpsilon * (1 + AbsReal(r)) * 100);
      x[i] := r;
      w[i] := -Exp(LnGamma(alpha + n, tsg) - LnGamma(n, tsg)) / (dp3 * n * p2);
      inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 2 - i do
        begin
          if AP_FP_Greater_Eq(x[j], x[j + 1]) then
            begin
              tmp := x[j];
              x[j] := x[j + 1];
              x[j + 1] := tmp;
              tmp := w[j];
              w[j] := w[j + 1];
              w[j + 1] := tmp;
            end;
          inc(j);
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Gauss-Hermite, another variant
  ************************************************************************ *)
procedure BuildGaussHermiteQuadrature(n: TLInt; var x: TLVec;
  var w: TLVec);
var
  i: TLInt;
  j: TLInt;
  r: TLFloat;
  r1: TLFloat;
  p1: TLFloat;
  p2: TLFloat;
  p3: TLFloat;
  dp3: TLFloat;
  pipm4: TLFloat;
  tmp: TLFloat;
begin
  SetLength(x, n);
  SetLength(w, n);
  pipm4 := Power(pi, -0.25);
  i := 0;
  while i <= (n + 1) div 2 - 1 do
    begin
      if i = 0 then
        begin
          r := Sqrt(2 * n + 1) - 1.85575 * Power(2 * n + 1, -AP_Float(1) / 6);
        end
      else
        begin
          if i = 1 then
            begin
              r := r - 1.14 * Power(n, 0.426) / r;
            end
          else
            begin
              if i = 2 then
                begin
                  r := 1.86 * r - 0.86 * x[0];
                end
              else
                begin
                  if i = 3 then
                    begin
                      r := 1.91 * r - 0.91 * x[1];
                    end
                  else
                    begin
                      r := 2 * r - x[i - 2];
                    end;
                end;
            end;
        end;
      repeat
        p2 := 0;
        p3 := pipm4;
        j := 0;
        while j <= n - 1 do
          begin
            p1 := p2;
            p2 := p3;
            p3 := p2 * r * Sqrt(AP_Float(2) / (j + 1)) - p1 *
              Sqrt(AP_Float(j) / (j + 1));
            inc(j);
          end;
        dp3 := Sqrt(2 * j) * p2;
        r1 := r;
        r := r - p3 / dp3;
      until AP_FP_Less(AbsReal(r - r1), MachineEpsilon * (1 + AbsReal(r)) * 100);
      x[i] := r;
      w[i] := 2 / (dp3 * dp3);
      x[n - 1 - i] := -x[i];
      w[n - 1 - i] := w[i];
      inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 2 - i do
        begin
          if AP_FP_Greater_Eq(x[j], x[j + 1]) then
            begin
              tmp := x[j];
              x[j] := x[j + 1];
              x[j + 1] := tmp;
              tmp := w[j];
              w[j] := w[j + 1];
              w[j + 1] := tmp;
            end;
          inc(j);
        end;
      inc(i);
    end;
end;

function TestGaussQuadrature(Silent: Boolean): Boolean;
(* ************************************************************************
  Maps:
  0   =>  -0.9
  1   =>  -0.5
  2   =>  -0.1
  3   =>   0.0
  4   =>  +0.1
  5   =>  +0.5
  6   =>  +0.9
  7   =>  +1.0
  8   =>  +1.5
  9   =>  +2.0
  ************************************************************************ *)
  function MapKind(k: TLInt): TLFloat;
  begin
    Result := 0;
    if k = 0 then
      begin
        Result := -0.9;
      end;
    if k = 1 then
      begin
        Result := -0.5;
      end;
    if k = 2 then
      begin
        Result := -0.1;
      end;
    if k = 3 then
      begin
        Result := 0.0;
      end;
    if k = 4 then
      begin
        Result := +0.1;
      end;
    if k = 5 then
      begin
        Result := +0.5;
      end;
    if k = 6 then
      begin
        Result := +0.9;
      end;
    if k = 7 then
      begin
        Result := +1.0;
      end;
    if k = 8 then
      begin
        Result := +1.5;
      end;
    if k = 9 then
      begin
        Result := +2.0;
      end;
  end;

var
  alpha: TLVec;
  beta: TLVec;
  x: TLVec;
  w: TLVec;
  x2: TLVec;
  W2: TLVec;
  Err: TLFloat;
  n: TLInt;
  i: TLInt;
  Info: TLInt;
  AKind: TLInt;
  BKind: TLInt;
  alphaC: TLFloat;
  betaC: TLFloat;
  ErrTol: TLFloat;
  NonStrictErrTol: TLFloat;
  StrictErrTol: TLFloat;
  RecErrors: Boolean;
  SpecErrors: Boolean;
  WasErrors: Boolean;
begin
  RecErrors := False;
  SpecErrors := False;
  WasErrors := False;
  ErrTol := 1.0E-12;
  NonStrictErrTol := 1.0E-6;
  StrictErrTol := 1000 * MachineEpsilon;

  //
  // Three tests for rec-based Gauss quadratures with known weights/nodes:
  // 1. Gauss-Legendre with N=2
  // 2. Gauss-Legendre with N=5
  // 3. Gauss-Chebyshev with N=1, 2, 4, 8, ..., 512
  //
  Err := 0;
  SetLength(alpha, 2);
  SetLength(beta, 2);

  // Alpha[0] := 1;
  // Alpha[1] := 1;
  // Beta[0] := 1;
  // Beta[1] := 1;
  // GaussQuadratureGenerateRec(Alpha, Beta, 1.0, 2, Info, x, w);

  alpha[0] := 0;
  alpha[1] := 0;
  beta[1] := AP_Float(1) / (4 * 1 * 1 - 1);
  GaussQuadratureGenerateRec(alpha, beta, 2.0, 2, Info, x, w);
  if Info > 0 then
    begin
      Err := Max(Err, AbsReal(x[0] + Sqrt(3) / 3));
      Err := Max(Err, AbsReal(x[1] - Sqrt(3) / 3));
      Err := Max(Err, AbsReal(w[0] - 1));
      Err := Max(Err, AbsReal(w[1] - 1));
      i := 0;
      while i <= 0 do
        begin
          RecErrors := RecErrors or AP_FP_Greater_Eq(x[i], x[i + 1]);
          inc(i);
        end;
    end
  else
    begin
      RecErrors := True;
    end;
  SetLength(alpha, 5);
  SetLength(beta, 5);
  alpha[0] := 0;
  i := 1;
  while i <= 4 do
    begin
      alpha[i] := 0;
      beta[i] := AP_Sqr(i) / (4 * AP_Sqr(i) - 1);
      inc(i);
    end;
  GaussQuadratureGenerateRec(alpha, beta, 2.0, 5, Info, x, w);
  if Info > 0 then
    begin
      Err := Max(Err, AbsReal(x[0] + Sqrt(245 + 14 * Sqrt(70)) / 21));
      Err := Max(Err, AbsReal(x[0] + x[4]));
      Err := Max(Err, AbsReal(x[1] + Sqrt(245 - 14 * Sqrt(70)) / 21));
      Err := Max(Err, AbsReal(x[1] + x[3]));
      Err := Max(Err, AbsReal(x[2]));
      Err := Max(Err, AbsReal(w[0] - (322 - 13 * Sqrt(70)) / 900));
      Err := Max(Err, AbsReal(w[0] - w[4]));
      Err := Max(Err, AbsReal(w[1] - (322 + 13 * Sqrt(70)) / 900));
      Err := Max(Err, AbsReal(w[1] - w[3]));
      Err := Max(Err, AbsReal(w[2] - AP_Float(128) / 225));
      i := 0;
      while i <= 3 do
        begin
          RecErrors := RecErrors or AP_FP_Greater_Eq(x[i], x[i + 1]);
          inc(i);
        end;
    end
  else
    begin
      RecErrors := True;
    end;
  n := 1;
  while n <= 512 do
    begin
      SetLength(alpha, n);
      SetLength(beta, n);
      i := 0;
      while i <= n - 1 do
        begin
          alpha[i] := 0;
          if i = 0 then
            begin
              beta[i] := 0;
            end;
          if i = 1 then
            begin
              beta[i] := AP_Float(1) / 2;
            end;
          if i > 1 then
            begin
              beta[i] := AP_Float(1) / 4;
            end;
          inc(i);
        end;
      GaussQuadratureGenerateRec(alpha, beta, pi, n, Info, x, w);
      if Info > 0 then
        begin
          i := 0;
          while i <= n - 1 do
            begin
              Err := Max(Err, AbsReal(x[i] - Cos(pi * (n - i - 0.5) / n)));
              Err := Max(Err, AbsReal(w[i] - pi / n));
              inc(i);
            end;
          i := 0;
          while i <= n - 2 do
            begin
              RecErrors := RecErrors or AP_FP_Greater_Eq(x[i], x[i + 1]);
              inc(i);
            end;
        end
      else
        begin
          RecErrors := True;
        end;
      n := n * 2;
    end;
  RecErrors := RecErrors or AP_FP_Greater(Err, ErrTol);

  //
  // Three tests for rec-based Gauss-Lobatto quadratures with known weights/nodes:
  // 1. Gauss-Lobatto with N=3
  // 2. Gauss-Lobatto with N=4
  // 3. Gauss-Lobatto with N=6
  //
  Err := 0;
  SetLength(alpha, 2);
  SetLength(beta, 2);
  alpha[0] := 0;
  alpha[1] := 0;
  beta[0] := 0;
  beta[1] := AP_Float(1 * 1) / (4 * 1 * 1 - 1);
  GaussQuadratureGenerateGaussLobattoRec(alpha, beta, 2.0, -1, +1, 3, Info, x, w);
  if Info > 0 then
    begin
      Err := Max(Err, AbsReal(x[0] + 1));
      Err := Max(Err, AbsReal(x[1]));
      Err := Max(Err, AbsReal(x[2] - 1));
      Err := Max(Err, AbsReal(w[0] - AP_Float(1) / 3));
      Err := Max(Err, AbsReal(w[1] - AP_Float(4) / 3));
      Err := Max(Err, AbsReal(w[2] - AP_Float(1) / 3));
      i := 0;
      while i <= 1 do
        begin
          RecErrors := RecErrors or AP_FP_Greater_Eq(x[i], x[i + 1]);
          inc(i);
        end;
    end
  else
    begin
      RecErrors := True;
    end;
  SetLength(alpha, 3);
  SetLength(beta, 3);
  alpha[0] := 0;
  alpha[1] := 0;
  alpha[2] := 0;
  beta[0] := 0;
  beta[1] := AP_Float(1 * 1) / (4 * 1 * 1 - 1);
  beta[2] := AP_Float(2 * 2) / (4 * 2 * 2 - 1);
  GaussQuadratureGenerateGaussLobattoRec(alpha, beta, 2.0, -1, +1, 4, Info, x, w);
  if Info > 0 then
    begin
      Err := Max(Err, AbsReal(x[0] + 1));
      Err := Max(Err, AbsReal(x[1] + Sqrt(5) / 5));
      Err := Max(Err, AbsReal(x[2] - Sqrt(5) / 5));
      Err := Max(Err, AbsReal(x[3] - 1));
      Err := Max(Err, AbsReal(w[0] - AP_Float(1) / 6));
      Err := Max(Err, AbsReal(w[1] - AP_Float(5) / 6));
      Err := Max(Err, AbsReal(w[2] - AP_Float(5) / 6));
      Err := Max(Err, AbsReal(w[3] - AP_Float(1) / 6));
      i := 0;
      while i <= 2 do
        begin
          RecErrors := RecErrors or AP_FP_Greater_Eq(x[i], x[i + 1]);
          inc(i);
        end;
    end
  else
    begin
      RecErrors := True;
    end;
  SetLength(alpha, 5);
  SetLength(beta, 5);
  alpha[0] := 0;
  alpha[1] := 0;
  alpha[2] := 0;
  alpha[3] := 0;
  alpha[4] := 0;
  beta[0] := 0;
  beta[1] := AP_Float(1 * 1) / (4 * 1 * 1 - 1);
  beta[2] := AP_Float(2 * 2) / (4 * 2 * 2 - 1);
  beta[3] := AP_Float(3 * 3) / (4 * 3 * 3 - 1);
  beta[4] := AP_Float(4 * 4) / (4 * 4 * 4 - 1);
  GaussQuadratureGenerateGaussLobattoRec(alpha, beta, 2.0, -1, +1, 6, Info, x, w);
  if Info > 0 then
    begin
      Err := Max(Err, AbsReal(x[0] + 1));
      Err := Max(Err, AbsReal(x[1] + Sqrt((7 + 2 * Sqrt(7)) / 21)));
      Err := Max(Err, AbsReal(x[2] + Sqrt((7 - 2 * Sqrt(7)) / 21)));
      Err := Max(Err, AbsReal(x[3] - Sqrt((7 - 2 * Sqrt(7)) / 21)));
      Err := Max(Err, AbsReal(x[4] - Sqrt((7 + 2 * Sqrt(7)) / 21)));
      Err := Max(Err, AbsReal(x[5] - 1));
      Err := Max(Err, AbsReal(w[0] - AP_Float(1) / 15));
      Err := Max(Err, AbsReal(w[1] - (14 - Sqrt(7)) / 30));
      Err := Max(Err, AbsReal(w[2] - (14 + Sqrt(7)) / 30));
      Err := Max(Err, AbsReal(w[3] - (14 + Sqrt(7)) / 30));
      Err := Max(Err, AbsReal(w[4] - (14 - Sqrt(7)) / 30));
      Err := Max(Err, AbsReal(w[5] - AP_Float(1) / 15));
      i := 0;
      while i <= 4 do
        begin
          RecErrors := RecErrors or AP_FP_Greater_Eq(x[i], x[i + 1]);
          inc(i);
        end;
    end
  else
    begin
      RecErrors := True;
    end;
  RecErrors := RecErrors or AP_FP_Greater(Err, ErrTol);

  //
  // Three tests for rec-based Gauss-Radau quadratures with known weights/nodes:
  // 1. Gauss-Radau with N=2
  // 2. Gauss-Radau with N=3
  // 3. Gauss-Radau with N=3 (another case)
  //
  Err := 0;
  SetLength(alpha, 1);
  SetLength(beta, 2);
  alpha[0] := 0;
  beta[0] := 0;
  beta[1] := AP_Float(1 * 1) / (4 * 1 * 1 - 1);
  GaussQuadratureGenerateGaussRadauRec(alpha, beta, 2.0, -1, 2, Info, x, w);
  if Info > 0 then
    begin
      Err := Max(Err, AbsReal(x[0] + 1));
      Err := Max(Err, AbsReal(x[1] - AP_Float(1) / 3));
      Err := Max(Err, AbsReal(w[0] - 0.5));
      Err := Max(Err, AbsReal(w[1] - 1.5));
      i := 0;
      while i <= 0 do
        begin
          RecErrors := RecErrors or AP_FP_Greater_Eq(x[i], x[i + 1]);
          inc(i);
        end;
    end
  else
    begin
      RecErrors := True;
    end;
  SetLength(alpha, 2);
  SetLength(beta, 3);
  alpha[0] := 0;
  alpha[1] := 0;
  i := 0;
  while i <= 2 do
    begin
      beta[i] := AP_Sqr(i) / (4 * AP_Sqr(i) - 1);
      inc(i);
    end;
  GaussQuadratureGenerateGaussRadauRec(alpha, beta, 2.0, -1, 3, Info, x, w);
  if Info > 0 then
    begin
      Err := Max(Err, AbsReal(x[0] + 1));
      Err := Max(Err, AbsReal(x[1] - (1 - Sqrt(6)) / 5));
      Err := Max(Err, AbsReal(x[2] - (1 + Sqrt(6)) / 5));
      Err := Max(Err, AbsReal(w[0] - AP_Float(2) / 9));
      Err := Max(Err, AbsReal(w[1] - (16 + Sqrt(6)) / 18));
      Err := Max(Err, AbsReal(w[2] - (16 - Sqrt(6)) / 18));
      i := 0;
      while i <= 1 do
        begin
          RecErrors := RecErrors or AP_FP_Greater_Eq(x[i], x[i + 1]);
          inc(i);
        end;
    end
  else
    begin
      RecErrors := True;
    end;
  SetLength(alpha, 2);
  SetLength(beta, 3);
  alpha[0] := 0;
  alpha[1] := 0;
  i := 0;
  while i <= 2 do
    begin
      beta[i] := AP_Sqr(i) / (4 * AP_Sqr(i) - 1);
      inc(i);
    end;
  GaussQuadratureGenerateGaussRadauRec(alpha, beta, 2.0, +1, 3, Info, x, w);
  if Info > 0 then
    begin
      Err := Max(Err, AbsReal(x[2] - 1));
      Err := Max(Err, AbsReal(x[1] + (1 - Sqrt(6)) / 5));
      Err := Max(Err, AbsReal(x[0] + (1 + Sqrt(6)) / 5));
      Err := Max(Err, AbsReal(w[2] - AP_Float(2) / 9));
      Err := Max(Err, AbsReal(w[1] - (16 + Sqrt(6)) / 18));
      Err := Max(Err, AbsReal(w[0] - (16 - Sqrt(6)) / 18));
      i := 0;
      while i <= 1 do
        begin
          RecErrors := RecErrors or AP_FP_Greater_Eq(x[i], x[i + 1]);
          inc(i);
        end;
    end
  else
    begin
      RecErrors := True;
    end;
  RecErrors := RecErrors or AP_FP_Greater(Err, ErrTol);

  //
  // test recurrence-based special cases (Legendre, Jacobi, Hermite, ...)
  // against another implementation (polynomial root-finder)

  //
  n := 1;
  while n <= 20 do
    begin

      //
      // test gauss-legendre
      //
      Err := 0;
      GaussQuadratureGenerateGaussLegendre(n, Info, x, w);
      if Info > 0 then
        begin
          BuildGaussLegendreQuadrature(n, x2, W2);
          i := 0;
          while i <= n - 1 do
            begin
              Err := Max(Err, AbsReal(x[i] - x2[i]));
              Err := Max(Err, AbsReal(w[i] - W2[i]));
              inc(i);
            end;
        end
      else
        begin
          SpecErrors := True;
        end;
      SpecErrors := SpecErrors or AP_FP_Greater(Err, ErrTol);

      //
      // Test Gauss-Jacobi.
      // Since task is much more difficult we will use less strict
      // threshold.
      //
      Err := 0;
      AKind := 0;
      while AKind <= 9 do
        begin
          BKind := 0;
          while BKind <= 9 do
            begin
              alphaC := MapKind(AKind);
              betaC := MapKind(BKind);
              GaussQuadratureGenerateGaussJacobi(n, alphaC, betaC, Info, x, w);
              if Info > 0 then
                begin
                  BuildGaussJacobiQuadrature(n, alphaC, betaC, x2, W2);
                  i := 0;
                  while i <= n - 1 do
                    begin
                      Err := Max(Err, AbsReal(x[i] - x2[i]));
                      Err := Max(Err, AbsReal(w[i] - W2[i]));
                      inc(i);
                    end;
                end
              else
                begin
                  SpecErrors := True;
                end;
              inc(BKind);
            end;
          inc(AKind);
        end;
      SpecErrors := SpecErrors or AP_FP_Greater(Err, NonStrictErrTol);

      //
      // special test for Gauss-Jacobi (Chebyshev weight
      // function with analytically known nodes/weights)
      //
      Err := 0;
      GaussQuadratureGenerateGaussJacobi(n, -0.5, -0.5, Info, x, w);
      if Info > 0 then
        begin
          i := 0;
          while i <= n - 1 do
            begin
              Err := Max(Err, AbsReal(x[i] + Cos(pi * (i + 0.5) / n)));
              Err := Max(Err, AbsReal(w[i] - pi / n));
              inc(i);
            end;
        end
      else
        begin
          SpecErrors := True;
        end;
      SpecErrors := SpecErrors or AP_FP_Greater(Err, StrictErrTol);

      //
      // Test Gauss-Laguerre
      //
      Err := 0;
      AKind := 0;
      while AKind <= 9 do
        begin
          alphaC := MapKind(AKind);
          GaussQuadratureGenerateGaussLaguerre(n, alphaC, Info, x, w);
          if Info > 0 then
            begin
              BuildGaussLaguerreQuadrature(n, alphaC, x2, W2);
              i := 0;
              while i <= n - 1 do
                begin
                  Err := Max(Err, AbsReal(x[i] - x2[i]));
                  Err := Max(Err, AbsReal(w[i] - W2[i]));
                  inc(i);
                end;
            end
          else
            begin
              SpecErrors := True;
            end;
          inc(AKind);
        end;
      SpecErrors := SpecErrors or AP_FP_Greater(Err, NonStrictErrTol);

      //
      // Test Gauss-Hermite
      //
      Err := 0;
      GaussQuadratureGenerateGaussHermite(n, Info, x, w);
      if Info > 0 then
        begin
          BuildGaussHermiteQuadrature(n, x2, W2);
          i := 0;
          while i <= n - 1 do
            begin
              Err := Max(Err, AbsReal(x[i] - x2[i]));
              Err := Max(Err, AbsReal(w[i] - W2[i]));
              inc(i);
            end;
        end
      else
        begin
          SpecErrors := True;
        end;
      SpecErrors := SpecErrors or AP_FP_Greater(Err, NonStrictErrTol);
      inc(n);
    end;

  //
  // end
  //
  WasErrors := RecErrors or SpecErrors;
  if not Silent then
    begin
      DoStatusNoLn(PFormat('TESTING GAUSS QUADRATURES'#13#10'', []));
      DoStatusNoLn(PFormat('FINAL RESULT:                             ', []));
      if WasErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* SPECIAL CASES (LEGENDRE/JACOBI/..)      ', []));
      if SpecErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* RECURRENCE-BASED:                       ', []));
      if RecErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      if WasErrors then
        begin
          DoStatusNoLn(PFormat('TEST FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('TEST PASSED'#13#10'', []));
        end;
      DoStatusNoLn(PFormat(''#13#10''#13#10'', []));
    end;
  Result := not WasErrors;
end;

function TestMinLBFGS(Silent: Boolean): Boolean;

(* ************************************************************************
  Calculate test function #1

  It may show very interesting behavior when optimized with 'x[0]>=ln(2)'
  constraint.
  ************************************************************************ *)
  procedure TestFunc1(var State: TMinLBFGSState);
  begin
    if AP_FP_Less(State.x[0], 100) then
      begin
        State.f := AP_Sqr(Exp(State.x[0]) - 2) + AP_Sqr(State.x[1]) +
          AP_Sqr(State.x[2] - State.x[0]);
        State.g[0] := 2 * (Exp(State.x[0]) - 2) * Exp(State.x[0]) + 2 *
          (State.x[0] - State.x[2]);
        State.g[1] := 2 * State.x[1];
        State.g[2] := 2 * (State.x[2] - State.x[0]);
      end
    else
      begin
        State.f := Sqrt(MaxRealNumber);
        State.g[0] := Sqrt(MaxRealNumber);
        State.g[1] := 0;
        State.g[2] := 0;
      end;
  end;

(* ************************************************************************
  Calculate test function #2

  Simple variation of #1, much more nonlinear, which makes unlikely premature
  convergence of algorithm .
  ************************************************************************ *)
  procedure TestFunc2(var State: TMinLBFGSState);
  begin
    if AP_FP_Less(State.x[0], 100) then
      begin
        State.f := AP_Sqr(Exp(State.x[0]) - 2) + AP_Sqr(AP_Sqr(State.x[1])) +
          AP_Sqr(State.x[2] - State.x[0]);
        State.g[0] := 2 * (Exp(State.x[0]) - 2) * Exp(State.x[0]) + 2 *
          (State.x[0] - State.x[2]);
        State.g[1] := 4 * State.x[1] * AP_Sqr(State.x[1]);
        State.g[2] := 2 * (State.x[2] - State.x[0]);
      end
    else
      begin
        State.f := Sqrt(MaxRealNumber);
        State.g[0] := Sqrt(MaxRealNumber);
        State.g[1] := 0;
        State.g[2] := 0;
      end;
  end;

(* ************************************************************************
  Calculate test function #3

  Simple variation of #1, much more nonlinear, with non-zero value at minimum.
  It achieve two goals:
  * makes unlikely premature convergence of algorithm .
  * solves some issues with EpsF stopping condition which arise when
  F(minimum) is zero

  ************************************************************************ *)
  procedure TestFunc3(var State: TMinLBFGSState);
  var
    s: TLFloat;
  begin
    s := 0.001;
    if AP_FP_Less(State.x[0], 100) then
      begin
        State.f := AP_Sqr(Exp(State.x[0]) - 2) + AP_Sqr(AP_Sqr(State.x[1]) + s) +
          AP_Sqr(State.x[2] - State.x[0]);
        State.g[0] := 2 * (Exp(State.x[0]) - 2) * Exp(State.x[0]) + 2 *
          (State.x[0] - State.x[2]);
        State.g[1] := 2 * (AP_Sqr(State.x[1]) + s) * 2 * State.x[1];
        State.g[2] := 2 * (State.x[2] - State.x[0]);
      end
    else
      begin
        State.f := Sqrt(MaxRealNumber);
        State.g[0] := Sqrt(MaxRealNumber);
        State.g[1] := 0;
        State.g[2] := 0;
      end;
  end;

var
  WasErrors: Boolean;
  RefError: Boolean;
  NonConvError: Boolean;
  EqError: Boolean;
  ConvError: Boolean;
  CrashTest: Boolean;
  OtherErrors: Boolean;
  n: TLInt;
  M: TLInt;
  x: TLVec;
  XE: TLVec;
  b: TLVec;
  XLast: TLVec;
  i: TLInt;
  j: TLInt;
  v: TLFloat;
  a: TLMatrix;
  MAXITS: TLInt;
  State: TMinLBFGSState;
  Rep: TMinLBFGSReport;
  FPrev: TLFloat;
  XPrev: TLFloat;
  StpMax: TLFloat;
begin
  WasErrors := False;

  //
  // Reference problem
  //
  SetLength(x, 2 + 1);
  n := 3;
  M := 2;
  x[0] := 100 * RandomReal - 50;
  x[1] := 100 * RandomReal - 50;
  x[2] := 100 * RandomReal - 50;
  MinLBFGSCreate(n, M, x, State);
  MinLBFGSSetCond(State, 0, 0, 0, 0);
  while MinLBFGSIteration(State) do
    begin
      State.f := AP_Sqr(State.x[0] - 2) + AP_Sqr(State.x[1]) +
        AP_Sqr(State.x[2] - State.x[0]);
      State.g[0] := 2 * (State.x[0] - 2) + 2 * (State.x[0] - State.x[2]);
      State.g[1] := 2 * State.x[1];
      State.g[2] := 2 * (State.x[2] - State.x[0]);
    end;
  MinLBFGSResults(State, x, Rep);
  RefError := (Rep.TerminationType <= 0) or AP_FP_Greater(AbsReal(x[0] - 2),
    0.001) or AP_FP_Greater(AbsReal(x[1]), 0.001) or
    AP_FP_Greater(AbsReal(x[2] - 2), 0.001);

  //
  // nonconvex problems with hard relief: we start from point with very small
  // gradient, but we need ever smaller gradient in the next step due to
  // Wolfe conditions.
  //
  NonConvError := False;
  SetLength(x, 1);
  n := 1;
  M := 1;
  v := -100;
  while AP_FP_Less(v, 0.1) do
    begin
      x[0] := v;
      MinLBFGSCreate(n, M, x, State);
      MinLBFGSSetCond(State, 1.0E-9, 0, 0, 0);
      while MinLBFGSIteration(State) do
        begin
          State.f := AP_Sqr(State.x[0]) / (1 + AP_Sqr(State.x[0]));
          State.g[0] := (2 * State.x[0] * (1 + AP_Sqr(State.x[0])) -
            AP_Sqr(State.x[0]) * 2 * State.x[0]) / AP_Sqr(1 + AP_Sqr(State.x[0]));
        end;
      MinLBFGSResults(State, x, Rep);
      NonConvError := NonConvError or (Rep.TerminationType <= 0) or
        AP_FP_Greater(AbsReal(x[0]), 0.001);
      v := v + 0.1;
    end;

  //
  // Linear equations
  //
  EqError := False;
  n := 1;
  while n <= 10 do
    begin

      //
      // Prepare task
      //
      SetLength(a, n (* - 1 + 1 // optimized compiler *) , n (* - 1 + 1 // optimized compiler *) );
      SetLength(x, n (* - 1 + 1 // optimized compiler *) );
      SetLength(XE, n (* - 1 + 1 // optimized compiler *) );
      SetLength(b, n (* - 1 + 1 // optimized compiler *) );
      i := 0;
      while i <= n - 1 do
        begin
          XE[i] := 2 * RandomReal - 1;
          inc(i);
        end;
      i := 0;
      while i <= n - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              a[i, j] := 2 * RandomReal - 1;
              inc(j);
            end;
          a[i, i] := a[i, i] + 3 * Sign(a[i, i]);
          inc(i);
        end;
      i := 0;
      while i <= n - 1 do
        begin
          v := APVDotProduct(@a[i][0], 0, n - 1, @XE[0], 0, n - 1);
          b[i] := v;
          inc(i);
        end;

      //
      // Test different M
      //
      M := 1;
      while M <= n do
        begin

          //
          // Solve task
          //
          i := 0;
          while i <= n - 1 do
            begin
              x[i] := 2 * RandomReal - 1;
              inc(i);
            end;
          MinLBFGSCreate(n, M, x, State);
          MinLBFGSSetCond(State, 0, 0, 0, 0);
          while MinLBFGSIteration(State) do
            begin
              State.f := 0;
              i := 0;
              while i <= n - 1 do
                begin
                  State.g[i] := 0;
                  inc(i);
                end;
              i := 0;
              while i <= n - 1 do
                begin
                  v := APVDotProduct(@a[i][0], 0, n - 1, @State.x[0], 0, n - 1);
                  State.f := State.f + AP_Sqr(v - b[i]);
                  j := 0;
                  while j <= n - 1 do
                    begin
                      State.g[j] := State.g[j] + 2 * (v - b[i]) * a[i, j];
                      inc(j);
                    end;
                  inc(i);
                end;
            end;
          MinLBFGSResults(State, x, Rep);
          EqError := EqError or (Rep.TerminationType <= 0);
          i := 0;
          while i <= n - 1 do
            begin
              EqError := EqError or AP_FP_Greater(AbsReal(x[i] - XE[i]), 0.001);
              inc(i);
            end;
          inc(M);
        end;
      inc(n);
    end;

  //
  // Testing convergence properties
  //
  ConvError := False;
  SetLength(x, 2 + 1);
  n := 3;
  M := 2;
  i := 0;
  while i <= 2 do
    begin
      x[i] := 6 * RandomReal - 3;
      inc(i);
    end;
  MinLBFGSCreate(n, M, x, State);
  MinLBFGSSetCond(State, 0.001, 0, 0, 0);
  while MinLBFGSIteration(State) do
    begin
      TestFunc3(State);
    end;
  MinLBFGSResults(State, x, Rep);
  ConvError := ConvError or (Rep.TerminationType <> 4);
  i := 0;
  while i <= 2 do
    begin
      x[i] := 6 * RandomReal - 3;
      inc(i);
    end;
  MinLBFGSCreate(n, M, x, State);
  MinLBFGSSetCond(State, 0, 0.001, 0, 0);
  while MinLBFGSIteration(State) do
    begin
      TestFunc3(State);
    end;
  MinLBFGSResults(State, x, Rep);
  ConvError := ConvError or (Rep.TerminationType <> 1);
  i := 0;
  while i <= 2 do
    begin
      x[i] := 6 * RandomReal - 3;
      inc(i);
    end;
  MinLBFGSCreate(n, M, x, State);
  MinLBFGSSetCond(State, 0, 0, 0.001, 0);
  while MinLBFGSIteration(State) do
    begin
      TestFunc3(State);
    end;
  MinLBFGSResults(State, x, Rep);
  ConvError := ConvError or (Rep.TerminationType <> 2);
  i := 0;
  while i <= 2 do
    begin
      x[i] := 2 * RandomReal - 1;
      inc(i);
    end;
  MinLBFGSCreate(n, M, x, State);
  MinLBFGSSetCond(State, 0, 0, 0, 10);
  while MinLBFGSIteration(State) do
    begin
      TestFunc3(State);
    end;
  MinLBFGSResults(State, x, Rep);
  ConvError := ConvError or (Rep.TerminationType <> 5) or
    (Rep.IterationsCount <> 10);

  //
  // Crash test: too many iterations on a simple tasks
  // May fail when encounter zero step, underflow or something like that
  //
  CrashTest := False;
  SetLength(x, 2 + 1);
  n := 3;
  M := 2;
  MAXITS := 10000;
  i := 0;
  while i <= 2 do
    begin
      x[i] := 6 * RandomReal - 3;
      inc(i);
    end;
  MinLBFGSCreate(n, M, x, State);
  MinLBFGSSetCond(State, 0, 0, 0, MAXITS);
  while MinLBFGSIteration(State) do
    begin
      State.f := AP_Sqr(Exp(State.x[0]) - 2) + AP_Sqr(State.x[1]) +
        AP_Sqr(State.x[2] - State.x[0]);
      State.g[0] := 2 * (Exp(State.x[0]) - 2) * Exp(State.x[0]) + 2 *
        (State.x[0] - State.x[2]);
      State.g[1] := 2 * State.x[1];
      State.g[2] := 2 * (State.x[2] - State.x[0]);
    end;
  MinLBFGSResults(State, x, Rep);
  CrashTest := CrashTest or (Rep.TerminationType <= 0);

  //
  // Other properties:
  // 1. test reports (F should form monotone sequence)
  // 2. test maximum step
  //
  OtherErrors := False;
  n := 50;
  M := 2;
  SetLength(x, n);
  SetLength(XLast, n);
  i := 0;
  while i <= n - 1 do
    begin
      x[i] := 1;
      inc(i);
    end;
  MinLBFGSCreate(n, M, x, State);
  MinLBFGSSetCond(State, 0, 0, 0, 100);
  MinLBFGSSetXRep(State, True);
  FPrev := MaxRealNumber;
  while MinLBFGSIteration(State) do
    begin
      if State.NeedFG then
        begin
          State.f := 0;
          i := 0;
          while i <= n - 1 do
            begin
              State.f := State.f + AP_Sqr((1 + i) * State.x[i]);
              State.g[i] := 2 * (1 + i) * State.x[i];
              inc(i);
            end;
        end;
      if State.XUpdated then
        begin
          OtherErrors := OtherErrors or AP_FP_Greater(State.f, FPrev);
          if AP_FP_Eq(FPrev, MaxRealNumber) then
            begin
              i := 0;
              while i <= n - 1 do
                begin
                  OtherErrors := OtherErrors or AP_FP_NEq(State.x[i], x[i]);
                  inc(i);
                end;
            end;
          FPrev := State.f;
          APVMove(@XLast[0], 0, n - 1, @State.x[0], 0, n - 1);
        end;
    end;
  MinLBFGSResults(State, x, Rep);
  i := 0;
  while i <= n - 1 do
    begin
      OtherErrors := OtherErrors or AP_FP_NEq(x[i], XLast[i]);
      inc(i);
    end;
  n := 1;
  M := 1;
  SetLength(x, n);
  x[0] := 100;
  StpMax := 0.05 + 0.05 * RandomReal;
  MinLBFGSCreate(n, M, x, State);
  MinLBFGSSetCond(State, 1.0E-9, 0, 0, 0);
  MinLBFGSSetStpMax(State, StpMax);
  MinLBFGSSetXRep(State, True);
  XPrev := x[0];
  while MinLBFGSIteration(State) do
    begin
      if State.NeedFG then
        begin
          State.f := Exp(State.x[0]) + Exp(-State.x[0]);
          State.g[0] := Exp(State.x[0]) - Exp(-State.x[0]);
          OtherErrors := OtherErrors or AP_FP_Greater(AbsReal(State.x[0] - XPrev),
            (1 + Sqrt(MachineEpsilon)) * StpMax);
        end;
      if State.XUpdated then
        begin
          OtherErrors := OtherErrors or AP_FP_Greater(AbsReal(State.x[0] - XPrev),
            (1 + Sqrt(MachineEpsilon)) * StpMax);
          XPrev := State.x[0];
        end;
    end;

  //
  // end
  //
  WasErrors := RefError or NonConvError or EqError or ConvError or CrashTest or
    OtherErrors;
  if not Silent then
    begin
      DoStatusNoLn(PFormat('TESTING L-BFGS OPTIMIZATION'#13#10'', []));
      DoStatusNoLn(PFormat('REFERENCE PROBLEM:                        ', []));
      if RefError then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('NON-CONVEX PROBLEM:                       ', []));
      if NonConvError then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('LINEAR EQUATIONS:                         ', []));
      if EqError then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('CONVERGENCE PROPERTIES:                   ', []));
      if ConvError then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('CRASH TEST:                               ', []));
      if CrashTest then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('OTHER PROPERTIES:                         ', []));
      if OtherErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      if WasErrors then
        begin
          DoStatusNoLn(PFormat('TEST FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('TEST PASSED'#13#10'', []));
        end;
      DoStatusNoLn(PFormat(''#13#10''#13#10'', []));
    end;
  Result := not WasErrors;
end;

function TestMinLM(Silent: Boolean): Boolean;
(* ************************************************************************
  Asserts that State fields are consistent with RKind.
  Returns False otherwise.
  ************************************************************************ *)
  function RKindVsStateCheck(RKind: TLInt; const State: TMinLMState): Boolean;
  var
    NSet: TLInt;
  begin
    NSet := 0;
    if State.NeedF then
      begin
        NSet := NSet + 1;
      end;
    if State.NeedFG then
      begin
        NSet := NSet + 1;
      end;
    if State.NeedFiJ then
      begin
        NSet := NSet + 1;
      end;
    if State.NeedFGH then
      begin
        NSet := NSet + 1;
      end;
    if State.XUpdated then
      begin
        NSet := NSet + 1;
      end;
    if NSet <> 1 then
      begin
        Result := False;
        Exit;
      end;
    if (RKind = 0) and (State.NeedFG or State.NeedFGH) then
      begin
        Result := False;
        Exit;
      end;
    if (RKind = 1) and State.NeedFGH then
      begin
        Result := False;
        Exit;
      end;
    if (RKind = 2) and State.NeedFiJ then
      begin
        Result := False;
        Exit;
      end;
    Result := True;
  end;

var
  WasErrors: Boolean;
  RefError: Boolean;
  Lin1Error: Boolean;
  Lin2Error: Boolean;
  EqError: Boolean;
  ConvError: Boolean;
  SCError: Boolean;
  OtherErrors: Boolean;
  RKind: TLInt;
  CKind: TLInt;
  EpsF: TLFloat;
  EpsX: TLFloat;
  EpsG: TLFloat;
  MAXITS: TLInt;
  n: TLInt;
  M: TLInt;
  x: TLVec;
  XE: TLVec;
  b: TLVec;
  XLast: TLVec;
  i: TLInt;
  j: TLInt;
  k: TLInt;
  v: TLFloat;
  s: TLFloat;
  StpMax: TLFloat;
  a: TLMatrix;
  FPrev: TLFloat;
  XPrev: TLFloat;
  State: TMinLMState;
  Rep: TMinLMReport;
begin
  WasErrors := False;
  RefError := False;
  Lin1Error := False;
  Lin2Error := False;
  EqError := False;
  ConvError := False;
  SCError := False;
  OtherErrors := False;

  //
  // Reference problem.
  // RKind is a algorithm selector:
  // * 0 = FJ
  // * 1 = FGJ
  // * 2 = FGH
  //
  SetLength(x, 2 + 1);
  n := 3;
  M := 3;
  RKind := 0;
  while RKind <= 2 do
    begin
      x[0] := 100 * RandomReal - 50;
      x[1] := 100 * RandomReal - 50;
      x[2] := 100 * RandomReal - 50;
      if RKind = 0 then
        begin
          MinLMCreateFJ(n, M, x, State);
        end;
      if RKind = 1 then
        begin
          MinLMCreateFGJ(n, M, x, State);
        end;
      if RKind = 2 then
        begin
          MinLMCreateFGH(n, x, State);
        end;
      while MinLMIteration(State) do
        begin

          //
          // (x-2)^2 + y^2 + (z-x)^2
          //
          State.f := AP_Sqr(State.x[0] - 2) + AP_Sqr(State.x[1]) +
            AP_Sqr(State.x[2] - State.x[0]);
          if State.NeedFG or State.NeedFGH then
            begin
              State.g[0] := 2 * (State.x[0] - 2) + 2 * (State.x[0] - State.x[2]);
              State.g[1] := 2 * State.x[1];
              State.g[2] := 2 * (State.x[2] - State.x[0]);
            end;
          if State.NeedFiJ then
            begin
              State.fi[0] := State.x[0] - 2;
              State.fi[1] := State.x[1];
              State.fi[2] := State.x[2] - State.x[0];
              State.j[0, 0] := 1;
              State.j[0, 1] := 0;
              State.j[0, 2] := 0;
              State.j[1, 0] := 0;
              State.j[1, 1] := 1;
              State.j[1, 2] := 0;
              State.j[2, 0] := -1;
              State.j[2, 1] := 0;
              State.j[2, 2] := 1;
            end;
          if State.NeedFGH then
            begin
              State.h[0, 0] := 4;
              State.h[0, 1] := 0;
              State.h[0, 2] := -2;
              State.h[1, 0] := 0;
              State.h[1, 1] := 2;
              State.h[1, 2] := 0;
              State.h[2, 0] := -2;
              State.h[2, 1] := 0;
              State.h[2, 2] := 2;
            end;
          SCError := SCError or not RKindVsStateCheck(RKind, State);
        end;
      MinLMResults(State, x, Rep);
      RefError := RefError or (Rep.TerminationType <= 0) or
        AP_FP_Greater(AbsReal(x[0] - 2), 0.001) or AP_FP_Greater(AbsReal(x[1]),
        0.001) or AP_FP_Greater(AbsReal(x[2] - 2), 0.001);
      inc(RKind);
    end;

  //
  // 1D problem #1
  //
  RKind := 0;
  while RKind <= 2 do
    begin
      SetLength(x, 1);
      n := 1;
      M := 1;
      x[0] := 100 * RandomReal - 50;
      if RKind = 0 then
        begin
          MinLMCreateFJ(n, M, x, State);
        end;
      if RKind = 1 then
        begin
          MinLMCreateFGJ(n, M, x, State);
        end;
      if RKind = 2 then
        begin
          MinLMCreateFGH(n, x, State);
        end;
      while MinLMIteration(State) do
        begin
          State.f := AP_Sqr(Sin(State.x[0]));
          if State.NeedFG or State.NeedFGH then
            begin
              State.g[0] := 2 * Sin(State.x[0]) * Cos(State.x[0]);
            end;
          if State.NeedFiJ then
            begin
              State.fi[0] := Sin(State.x[0]);
              State.j[0, 0] := Cos(State.x[0]);
            end;
          if State.NeedFGH then
            begin
              State.h[0, 0] := 2 * (Cos(State.x[0]) * Cos(State.x[0]) - Sin(State.x[0]
                ) * Sin(State.x[0]));
            end;
          SCError := SCError or not RKindVsStateCheck(RKind, State);
        end;
      MinLMResults(State, x, Rep);
      Lin1Error := (Rep.TerminationType <= 0) or
        AP_FP_Greater(AbsReal(x[0] / pi - Round(x[0] / pi)), 0.001);
      inc(RKind);
    end;

  //
  // Linear equations
  //
  n := 1;
  while n <= 10 do
    begin

      //
      // Prepare task
      //
      RMatrixRndCond(n, 100, a);
      SetLength(x, n);
      SetLength(XE, n);
      SetLength(b, n);
      i := 0;
      while i <= n - 1 do
        begin
          XE[i] := 2 * RandomReal - 1;
          inc(i);
        end;
      i := 0;
      while i <= n - 1 do
        begin
          v := APVDotProduct(@a[i][0], 0, n - 1, @XE[0], 0, n - 1);
          b[i] := v;
          inc(i);
        end;

      //
      // Test different RKind
      //
      RKind := 0;
      while RKind <= 2 do
        begin

          //
          // Solve task
          //
          i := 0;
          while i <= n - 1 do
            begin
              x[i] := 2 * RandomReal - 1;
              inc(i);
            end;
          if RKind = 0 then
            begin
              MinLMCreateFJ(n, n, x, State);
            end;
          if RKind = 1 then
            begin
              MinLMCreateFGJ(n, n, x, State);
            end;
          if RKind = 2 then
            begin
              MinLMCreateFGH(n, x, State);
            end;
          while MinLMIteration(State) do
            begin
              if State.NeedF or State.NeedFG or State.NeedFGH then
                begin
                  State.f := 0;
                end;
              if State.NeedFG or State.NeedFGH then
                begin
                  i := 0;
                  while i <= n - 1 do
                    begin
                      State.g[i] := 0;
                      inc(i);
                    end;
                end;
              if State.NeedFGH then
                begin
                  i := 0;
                  while i <= n - 1 do
                    begin
                      j := 0;
                      while j <= n - 1 do
                        begin
                          State.h[i, j] := 0;
                          inc(j);
                        end;
                      inc(i);
                    end;
                end;
              i := 0;
              while i <= n - 1 do
                begin
                  v := APVDotProduct(@a[i][0], 0, n - 1, @State.x[0], 0, n - 1);
                  if State.NeedF or State.NeedFG or State.NeedFGH then
                    begin
                      State.f := State.f + AP_Sqr(v - b[i]);
                    end;
                  if State.NeedFG or State.NeedFGH then
                    begin
                      j := 0;
                      while j <= n - 1 do
                        begin
                          State.g[j] := State.g[j] + 2 * (v - b[i]) * a[i, j];
                          inc(j);
                        end;
                    end;
                  if State.NeedFGH then
                    begin
                      j := 0;
                      while j <= n - 1 do
                        begin
                          k := 0;
                          while k <= n - 1 do
                            begin
                              State.h[j, k] := State.h[j, k] + 2 * a[i, j] * a[i, k];
                              inc(k);
                            end;
                          inc(j);
                        end;
                    end;
                  if State.NeedFiJ then
                    begin
                      State.fi[i] := v - b[i];
                      APVMove(@State.j[i][0], 0, n - 1, @a[i][0], 0, n - 1);
                    end;
                  inc(i);
                end;
              SCError := SCError or not RKindVsStateCheck(RKind, State);
            end;
          MinLMResults(State, x, Rep);
          EqError := EqError or (Rep.TerminationType <= 0);
          i := 0;
          while i <= n - 1 do
            begin
              EqError := EqError or AP_FP_Greater(AbsReal(x[i] - XE[i]), 0.001);
              inc(i);
            end;
          inc(RKind);
        end;
      inc(n);
    end;

  //
  // Testing convergence properties using
  // different optimizer types and different conditions
  //
  s := 100;
  RKind := 0;
  while RKind <= 2 do
    begin
      CKind := 0;
      while CKind <= 3 do
        begin
          EpsG := 0;
          EpsF := 0;
          EpsX := 0;
          MAXITS := 0;
          if CKind = 0 then
            begin
              EpsF := 0.0001;
            end;
          if CKind = 1 then
            begin
              EpsX := 0.0001;
            end;
          if CKind = 2 then
            begin
              MAXITS := 2;
            end;
          if CKind = 3 then
            begin
              EpsG := 0.0001;
            end;
          SetLength(x, 3);
          n := 3;
          M := 3;
          i := 0;
          while i <= 2 do
            begin
              x[i] := 6;
              inc(i);
            end;
          if RKind = 0 then
            begin
              MinLMCreateFJ(n, M, x, State);
            end;
          if RKind = 1 then
            begin
              MinLMCreateFGJ(n, M, x, State);
            end;
          if RKind = 2 then
            begin
              MinLMCreateFGH(n, x, State);
            end;
          MinLMSetCond(State, EpsG, EpsF, EpsX, MAXITS);
          while MinLMIteration(State) do
            begin
              if State.NeedF or State.NeedFG or State.NeedFGH then
                begin
                  State.f := s * AP_Sqr(Exp(State.x[0]) - 2) +
                    AP_Sqr(AP_Sqr(State.x[1]) + 1) + AP_Sqr(State.x[2] - State.x[0]);
                end;
              if State.NeedFG or State.NeedFGH then
                begin
                  State.g[0] := s * 2 * (Exp(State.x[0]) - 2) * Exp(State.x[0]) + 2 *
                    (State.x[0] - State.x[2]);
                  State.g[1] := 2 * (AP_Sqr(State.x[1]) + 1) * 2 * State.x[1];
                  State.g[2] := 2 * (State.x[2] - State.x[0]);
                end;
              if State.NeedFGH then
                begin
                  State.h[0, 0] := s * (4 * AP_Sqr(Exp(State.x[0])) - 4 *
                    Exp(State.x[0])) + 2;
                  State.h[0, 1] := 0;
                  State.h[0, 2] := -2;
                  State.h[1, 0] := 0;
                  State.h[1, 1] := 12 * AP_Sqr(State.x[1]) + 4;
                  State.h[1, 2] := 0;
                  State.h[2, 0] := -2;
                  State.h[2, 1] := 0;
                  State.h[2, 2] := 2;
                end;
              if State.NeedFiJ then
                begin
                  State.fi[0] := s * (Exp(State.x[0]) - 2);
                  State.j[0, 0] := s * Exp(State.x[0]);
                  State.j[0, 1] := 0;
                  State.j[0, 2] := 0;
                  State.fi[1] := AP_Sqr(State.x[1]) + 1;
                  State.j[1, 0] := 0;
                  State.j[1, 1] := 2 * State.x[1];
                  State.j[1, 2] := 0;
                  State.fi[2] := State.x[2] - State.x[0];
                  State.j[2, 0] := -1;
                  State.j[2, 1] := 0;
                  State.j[2, 2] := 1;
                end;
              SCError := SCError or not RKindVsStateCheck(RKind, State);
            end;
          MinLMResults(State, x, Rep);
          if CKind = 0 then
            begin
              ConvError := ConvError or AP_FP_Greater(AbsReal(x[0] - ln(2)), 0.05);
              ConvError := ConvError or AP_FP_Greater(AbsReal(x[1]), 0.05);
              ConvError := ConvError or AP_FP_Greater(AbsReal(x[2] - ln(2)), 0.05);
              ConvError := ConvError or (Rep.TerminationType <> 1);
            end;
          if CKind = 1 then
            begin
              ConvError := ConvError or AP_FP_Greater(AbsReal(x[0] - ln(2)), 0.05);
              ConvError := ConvError or AP_FP_Greater(AbsReal(x[1]), 0.05);
              ConvError := ConvError or AP_FP_Greater(AbsReal(x[2] - ln(2)), 0.05);
              ConvError := ConvError or (Rep.TerminationType <> 2);
            end;
          if CKind = 2 then
            begin
              ConvError := ConvError or (Rep.TerminationType <> 5) or
                (Rep.IterationsCount <> MAXITS);
            end;
          if CKind = 3 then
            begin
              ConvError := ConvError or AP_FP_Greater(AbsReal(x[0] - ln(2)), 0.05);
              ConvError := ConvError or AP_FP_Greater(AbsReal(x[1]), 0.05);
              ConvError := ConvError or AP_FP_Greater(AbsReal(x[2] - ln(2)), 0.05);
              ConvError := ConvError or (Rep.TerminationType <> 4);
            end;
          inc(CKind);
        end;
      inc(RKind);
    end;

  //
  // Other properties:
  // 1. test reports (F should form monotone sequence)
  // 2. test maximum step
  //
  RKind := 0;
  while RKind <= 2 do
    begin

      //
      // reports:
      // * check that first report is initial point
      // * check that F is monotone decreasing
      // * check that last report is final result
      //
      n := 3;
      M := 3;
      s := 100;
      SetLength(x, n);
      SetLength(XLast, n);
      i := 0;
      while i <= n - 1 do
        begin
          x[i] := 6;
          inc(i);
        end;
      if RKind = 0 then
        begin
          MinLMCreateFJ(n, M, x, State);
        end;
      if RKind = 1 then
        begin
          MinLMCreateFGJ(n, M, x, State);
        end;
      if RKind = 2 then
        begin
          MinLMCreateFGH(n, x, State);
        end;
      MinLMSetCond(State, 0, 0, 0, 4);
      MinLMSetXRep(State, True);
      FPrev := MaxRealNumber;
      while MinLMIteration(State) do
        begin
          if State.NeedF or State.NeedFG or State.NeedFGH then
            begin
              State.f := s * AP_Sqr(Exp(State.x[0]) - 2) + AP_Sqr(State.x[1]) +
                AP_Sqr(State.x[2] - State.x[0]);
            end;
          if State.NeedFG or State.NeedFGH then
            begin
              State.g[0] := s * 2 * (Exp(State.x[0]) - 2) * Exp(State.x[0]) + 2 *
                (State.x[0] - State.x[2]);
              State.g[1] := 2 * State.x[1];
              State.g[2] := 2 * (State.x[2] - State.x[0]);
            end;
          if State.NeedFGH then
            begin
              State.h[0, 0] := s * (4 * AP_Sqr(Exp(State.x[0])) - 4 *
                Exp(State.x[0])) + 2;
              State.h[0, 1] := 0;
              State.h[0, 2] := -2;
              State.h[1, 0] := 0;
              State.h[1, 1] := 2;
              State.h[1, 2] := 0;
              State.h[2, 0] := -2;
              State.h[2, 1] := 0;
              State.h[2, 2] := 2;
            end;
          if State.NeedFiJ then
            begin
              State.fi[0] := Sqrt(s) * (Exp(State.x[0]) - 2);
              State.j[0, 0] := Sqrt(s) * Exp(State.x[0]);
              State.j[0, 1] := 0;
              State.j[0, 2] := 0;
              State.fi[1] := State.x[1];
              State.j[1, 0] := 0;
              State.j[1, 1] := 1;
              State.j[1, 2] := 0;
              State.fi[2] := State.x[2] - State.x[0];
              State.j[2, 0] := -1;
              State.j[2, 1] := 0;
              State.j[2, 2] := 1;
            end;
          SCError := SCError or not RKindVsStateCheck(RKind, State);
          if State.XUpdated then
            begin
              OtherErrors := OtherErrors or AP_FP_Greater(State.f, FPrev);
              if AP_FP_Eq(FPrev, MaxRealNumber) then
                begin
                  i := 0;
                  while i <= n - 1 do
                    begin
                      OtherErrors := OtherErrors or AP_FP_NEq(State.x[i], x[i]);
                      inc(i);
                    end;
                end;
              FPrev := State.f;
              APVMove(@XLast[0], 0, n - 1, @State.x[0], 0, n - 1);
            end;
        end;
      MinLMResults(State, x, Rep);
      i := 0;
      while i <= n - 1 do
        begin
          OtherErrors := OtherErrors or AP_FP_NEq(x[i], XLast[i]);
          inc(i);
        end;
      inc(RKind);
    end;
  n := 1;
  SetLength(x, n);
  x[0] := 100;
  StpMax := 0.05 + 0.05 * RandomReal;
  MinLMCreateFGH(n, x, State);
  MinLMSetCond(State, 1.0E-9, 0, 0, 0);
  MinLMSetStpMax(State, StpMax);
  MinLMSetXRep(State, True);
  XPrev := x[0];
  while MinLMIteration(State) do
    begin
      if State.NeedF or State.NeedFG or State.NeedFGH then
        begin
          State.f := Exp(State.x[0]) + Exp(-State.x[0]);
        end;
      if State.NeedFG or State.NeedFGH then
        begin
          State.g[0] := Exp(State.x[0]) - Exp(-State.x[0]);
        end;
      if State.NeedFGH then
        begin
          State.h[0, 0] := Exp(State.x[0]) + Exp(-State.x[0]);
        end;
      OtherErrors := OtherErrors or AP_FP_Greater(AbsReal(State.x[0] - XPrev),
        (1 + Sqrt(MachineEpsilon)) * StpMax);
      if State.XUpdated then
        begin
          XPrev := State.x[0];
        end;
    end;

  //
  // end
  //
  WasErrors := RefError or Lin1Error or Lin2Error or EqError or ConvError or SCError or OtherErrors;
  if not Silent then
    begin
      DoStatusNoLn(PFormat('TESTING LEVENBERG-MARQUARDT OPTIMIZATION'#13#10'', []));
      DoStatusNoLn(PFormat('REFERENCE PROBLEM:                        ', []));
      if RefError then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('1-D PROBLEM #1:                           ', []));
      if Lin1Error then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('1-D PROBLEM #2:                           ', []));
      if Lin2Error then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('LINEAR EQUATIONS:                         ', []));
      if EqError then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('CONVERGENCE PROPERTIES:                   ', []));
      if ConvError then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('STATE FIELDS CONSISTENCY:                 ', []));
      if SCError then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('OTHER PROPERTIES:                         ', []));
      if OtherErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      if WasErrors then
        begin
          DoStatusNoLn(PFormat('TEST FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('TEST PASSED'#13#10'', []));
        end;
      DoStatusNoLn(PFormat(''#13#10''#13#10'', []));
    end;
  Result := not WasErrors;
end;

function TestLLS(Silent: Boolean): Boolean;

(* ************************************************************************
  Tests whether C is solution of LLS problem
  ************************************************************************ *)
  function GetGLSError(n: TLInt; M: TLInt; const y: TLVec;
    const w: TLVec; const FMatrix: TLMatrix; const c: TLVec)
    : TLFloat;
  var
    i: TLInt;
    v: TLFloat;
  begin
    Result := 0;
    i := 0;
    while i <= n - 1 do
      begin
        v := APVDotProduct(@FMatrix[i][0], 0, M - 1, @c[0], 0, M - 1);
        Result := Result + AP_Sqr(w[i] * (v - y[i]));
        inc(i);
      end;
  end;

(* ************************************************************************
  Tests whether C is solution of (possibly) constrained LLS problem
  ************************************************************************ *)
  function IsGLSSolution(n: TLInt; M: TLInt; k: TLInt;
    const y: TLVec; const w: TLVec; const FMatrix: TLMatrix;
    const CMatrix: TLMatrix; c: TLVec): Boolean;
  var
    i: TLInt;
    j: TLInt;
    c2: TLVec;
    SV: TLVec;
    DeltaC: TLVec;
    DeltaProj: TLVec;
    u: TLMatrix;
    VT: TLMatrix;
    v: TLFloat;
    s1: TLFloat;
    s2: TLFloat;
    s3: TLFloat;
    Delta: TLFloat;
    Threshold: TLFloat;
  begin
    c := DynamicArrayCopy(c);

    //
    // Setup.
    // Threshold is small because CMatrix may be ill-conditioned
    //
    Delta := 0.001;
    Threshold := Sqrt(MachineEpsilon);
    SetLength(c2, M);
    SetLength(DeltaC, M);
    SetLength(DeltaProj, M);

    //
    // test whether C is feasible point or not (projC must be close to C)
    //
    i := 0;
    while i <= k - 1 do
      begin
        v := APVDotProduct(@CMatrix[i][0], 0, M - 1, @c[0], 0, M - 1);
        if AP_FP_Greater(AbsReal(v - CMatrix[i, M]), Threshold) then
          begin
            Result := False;
            Exit;
          end;
        inc(i);
      end;

    //
    // find orthogonal basis of Null(CMatrix) (stored in rows from K to M-1)
    //
    if k > 0 then
      begin
        RMatrixSVD(CMatrix, k, M, 0, 2, 2, SV, u, VT);
      end;

    //
    // Test result
    //
    Result := True;
    s1 := GetGLSError(n, M, y, w, FMatrix, c);
    j := 0;
    while j <= M - 1 do
      begin

        //
        // prepare modification of C which leave us in the feasible set.
        //
        // let deltaC be increment on Jth coordinate, then project
        // deltaC in the Null(CMatrix) and store result in DeltaProj
        //
        APVMove(@c2[0], 0, M - 1, @c[0], 0, M - 1);
        i := 0;
        while i <= M - 1 do
          begin
            if i = j then
              begin
                DeltaC[i] := Delta;
              end
            else
              begin
                DeltaC[i] := 0;
              end;
            inc(i);
          end;
        if k = 0 then
          begin
            APVMove(@DeltaProj[0], 0, M - 1, @DeltaC[0], 0, M - 1);
          end
        else
          begin
            i := 0;
            while i <= M - 1 do
              begin
                DeltaProj[i] := 0;
                inc(i);
              end;
            i := k;
            while i <= M - 1 do
              begin
                v := APVDotProduct(@VT[i][0], 0, M - 1, @DeltaC[0], 0, M - 1);
                APVAdd(@DeltaProj[0], 0, M - 1, @VT[i][0], 0, M - 1, v);
                inc(i);
              end;
          end;

        //
        // now we have DeltaProj such that if C is feasible,
        // then C+DeltaProj is feasible too
        //
        APVMove(@c2[0], 0, M - 1, @c[0], 0, M - 1);
        APVAdd(@c2[0], 0, M - 1, @DeltaProj[0], 0, M - 1);
        s2 := GetGLSError(n, M, y, w, FMatrix, c2);
        APVMove(@c2[0], 0, M - 1, @c[0], 0, M - 1);
        APVSub(@c2[0], 0, M - 1, @DeltaProj[0], 0, M - 1);
        s3 := GetGLSError(n, M, y, w, FMatrix, c2);
        Result := Result and AP_FP_Greater_Eq(s2, s1 / (1 + Threshold)) and
          AP_FP_Greater_Eq(s3, s1 / (1 + Threshold));
        inc(j);
      end;
  end;

(* ************************************************************************
  Subroutine for nonlinear fitting of linear problem
  ************************************************************************ *)
  procedure FitLinearNonlinear(M: TLInt; GradOnly: Boolean;
    const xy: TLMatrix; var State: TLSFitState; var NLSErrors: Boolean);
  var
    i: TLInt;
    j: TLInt;
    v: TLFloat;
  begin
    while LSFitNonlinearIteration(State) do
      begin
        //
        // assume that one and only one of flags is set
        // test that we didn't request hessian in hessian-free setting
        //
        if GradOnly and State.NeedFGH then
          begin
            NLSErrors := True;
          end;
        i := 0;
        if State.NeedF then
          begin
            i := i + 1;
          end;
        if State.NeedFG then
          begin
            i := i + 1;
          end;
        if State.NeedFGH then
          begin
            i := i + 1;
          end;
        if i <> 1 then
          begin
            NLSErrors := True;
          end;

        //
        // test that PointIndex is consistent with actual point passed
        //
        i := 0;
        while i <= M - 1 do
          begin
            NLSErrors := NLSErrors or AP_FP_NEq(xy[State.PointIndex, i], State.x[i]);
            inc(i);
          end;

        //
        // calculate
        //
        if State.NeedF then
          begin
            v := APVDotProduct(@State.x[0], 0, M - 1, @State.c[0], 0, M - 1);
            State.f := v;
            Continue;
          end;
        if State.NeedFG then
          begin
            v := APVDotProduct(@State.x[0], 0, M - 1, @State.c[0], 0, M - 1);
            State.f := v;
            APVMove(@State.g[0], 0, M - 1, @State.x[0], 0, M - 1);
            Continue;
          end;
        if State.NeedFGH then
          begin
            v := APVDotProduct(@State.x[0], 0, M - 1, @State.c[0], 0, M - 1);
            State.f := v;
            APVMove(@State.g[0], 0, M - 1, @State.x[0], 0, M - 1);
            i := 0;
            while i <= M - 1 do
              begin
                j := 0;
                while j <= M - 1 do
                  begin
                    State.h[i, j] := 0;
                    inc(j);
                  end;
                inc(i);
              end;
            Continue;
          end;
      end;
  end;

var
  WasErrors: Boolean;
  LLSErrors: Boolean;
  NLSErrors: Boolean;
  Threshold: TLFloat;
  NLThreshold: TLFloat;
  MaxN: TLInt;
  MaxM: TLInt;
  PassCount: TLInt;
  n: TLInt;
  M: TLInt;
  i: TLInt;
  j: TLInt;
  k: TLInt;
  pass: TLInt;
  XScale: TLFloat;
  x: TLVec;
  y: TLVec;
  w: TLVec;
  W2: TLVec;
  c: TLVec;
  c2: TLVec;
  a: TLMatrix;
  a2: TLMatrix;
  CM: TLMatrix;
  v: TLFloat;
  v1: TLFloat;
  v2: TLFloat;
  Rep: TLSFitReport;
  Rep2: TLSFitReport;
  Info: TLInt;
  Info2: TLInt;
  RefRMS: TLFloat;
  RefAvg: TLFloat;
  RefAvgRel: TLFloat;
  RefMax: TLFloat;
  State: TLSFitState;
begin
  WasErrors := False;
  LLSErrors := False;
  NLSErrors := False;
  Threshold := 10000 * MachineEpsilon;
  NLThreshold := 0.00001;
  MaxN := 6;
  MaxM := 6;
  PassCount := 4;

  //
  // Testing unconstrained least squares (linear/nonlinear)
  //
  n := 1;
  while n <= MaxN do
    begin
      M := 1;
      while M <= MaxM do
        begin
          pass := 1;
          while pass <= PassCount do
            begin

              //
              // Solve non-degenerate linear least squares task
              // Use Chebyshev basis. Its condition number is very good.
              //
              SetLength(a, n, M);
              SetLength(x, n);
              SetLength(y, n);
              SetLength(w, n);
              XScale := 0.9 + 0.1 * RandomReal;
              i := 0;
              while i <= n - 1 do
                begin
                  if n = 1 then
                    begin
                      x[i] := 2 * RandomReal - 1;
                    end
                  else
                    begin
                      x[i] := XScale * (AP_Float(2 * i) / (n - 1) - 1);
                    end;
                  y[i] := 3 * x[i] + Exp(x[i]);
                  w[i] := 1 + RandomReal;
                  a[i, 0] := 1;
                  if M > 1 then
                    begin
                      a[i, 1] := x[i];
                    end;
                  j := 2;
                  while j <= M - 1 do
                    begin
                      a[i, j] := 2 * x[i] * a[i, j - 1] - a[i, j - 2];
                      inc(j);
                    end;
                  inc(i);
                end;

              //
              // 1. test weighted fitting (optimality)
              // 2. Solve degenerate least squares task built on the basis
              // of previous task
              //
              LSFitLinearW(y, w, a, n, M, Info, c, Rep);
              if Info <= 0 then
                begin
                  LLSErrors := True;
                end
              else
                begin
                  LLSErrors := LLSErrors or not IsGLSSolution(n, M, 0, y, w, a, CM, c);
                end;
              SetLength(a2, n, 2 * M);
              i := 0;
              while i <= n - 1 do
                begin
                  j := 0;
                  while j <= M - 1 do
                    begin
                      a2[i, 2 * j + 0] := a[i, j];
                      a2[i, 2 * j + 1] := a[i, j];
                      inc(j);
                    end;
                  inc(i);
                end;
              LSFitLinearW(y, w, a2, n, 2 * M, Info, c2, Rep);
              if Info <= 0 then
                begin
                  LLSErrors := True;
                end
              else
                begin

                  //
                  // test answer correctness using design matrix properties
                  // and previous task solution
                  //
                  j := 0;
                  while j <= M - 1 do
                    begin
                      LLSErrors := LLSErrors or
                        AP_FP_Greater(AbsReal(c2[2 * j + 0] + c2[2 * j + 1] - c[j]),
                        Threshold);
                      inc(j);
                    end;
                end;

              //
              // test non-weighted fitting
              //
              SetLength(W2, n);
              i := 0;
              while i <= n - 1 do
                begin
                  W2[i] := 1;
                  inc(i);
                end;
              LSFitLinearW(y, W2, a, n, M, Info, c, Rep);
              LSFitLinear(y, a, n, M, Info2, c2, Rep2);
              if (Info <= 0) or (Info2 <= 0) then
                begin
                  LLSErrors := True;
                end
              else
                begin

                  //
                  // test answer correctness
                  //
                  j := 0;
                  while j <= M - 1 do
                    begin
                      LLSErrors := LLSErrors or AP_FP_Greater(AbsReal(c[j] - c2[j]),
                        Threshold);
                      inc(j);
                    end;
                  LLSErrors := LLSErrors or
                    AP_FP_Greater(AbsReal(Rep.TaskRCond - Rep2.TaskRCond), Threshold);
                end;

              //
              // test nonlinear fitting on the linear task
              // (only non-degenerate task are tested)
              // and compare with answer from linear fitting subroutine
              //
              if n >= M then
                begin
                  SetLength(c2, M);

                  //
                  // test gradient-only or Hessian-based weighted fitting
                  //
                  LSFitLinearW(y, w, a, n, M, Info, c, Rep);
                  i := 0;
                  while i <= M - 1 do
                    begin
                      c2[i] := 2 * RandomReal - 1;
                      inc(i);
                    end;
                  LSFitNonlinearWFG(a, y, w, c2, n, M, M, AP_FP_Greater(RandomReal,
                    0.5), State);
                  LSFitNonlinearSetCond(State, 0.0, NLThreshold, 0);
                  FitLinearNonlinear(M, True, a, State, NLSErrors);
                  LSFitNonlinearResults(State, Info, c2, Rep2);
                  if Info <= 0 then
                    begin
                      NLSErrors := True;
                    end
                  else
                    begin
                      i := 0;
                      while i <= M - 1 do
                        begin
                          NLSErrors := NLSErrors or AP_FP_Greater(AbsReal(c[i] - c2[i]),
                            100 * NLThreshold);
                          inc(i);
                        end;
                    end;
                  i := 0;
                  while i <= M - 1 do
                    begin
                      c2[i] := 2 * RandomReal - 1;
                      inc(i);
                    end;
                  LSFitNonlinearWFGH(a, y, w, c2, n, M, M, State);
                  LSFitNonlinearSetCond(State, 0.0, NLThreshold, 0);
                  FitLinearNonlinear(M, False, a, State, NLSErrors);
                  LSFitNonlinearResults(State, Info, c2, Rep2);
                  if Info <= 0 then
                    begin
                      NLSErrors := True;
                    end
                  else
                    begin
                      i := 0;
                      while i <= M - 1 do
                        begin
                          NLSErrors := NLSErrors or AP_FP_Greater(AbsReal(c[i] - c2[i]),
                            100 * NLThreshold);
                          inc(i);
                        end;
                    end;

                  //
                  // test gradient-only or Hessian-based fitting without weights
                  //
                  LSFitLinear(y, a, n, M, Info, c, Rep);
                  i := 0;
                  while i <= M - 1 do
                    begin
                      c2[i] := 2 * RandomReal - 1;
                      inc(i);
                    end;
                  LSFitNonlinearFG(a, y, c2, n, M, M, AP_FP_Greater(RandomReal,
                    0.5), State);
                  LSFitNonlinearSetCond(State, 0.0, NLThreshold, 0);
                  FitLinearNonlinear(M, True, a, State, NLSErrors);
                  LSFitNonlinearResults(State, Info, c2, Rep2);
                  if Info <= 0 then
                    begin
                      NLSErrors := True;
                    end
                  else
                    begin
                      i := 0;
                      while i <= M - 1 do
                        begin
                          NLSErrors := NLSErrors or AP_FP_Greater(AbsReal(c[i] - c2[i]),
                            100 * NLThreshold);
                          inc(i);
                        end;
                    end;
                  i := 0;
                  while i <= M - 1 do
                    begin
                      c2[i] := 2 * RandomReal - 1;
                      inc(i);
                    end;
                  LSFitNonlinearFGH(a, y, c2, n, M, M, State);
                  LSFitNonlinearSetCond(State, 0.0, NLThreshold, 0);
                  FitLinearNonlinear(M, False, a, State, NLSErrors);
                  LSFitNonlinearResults(State, Info, c2, Rep2);
                  if Info <= 0 then
                    begin
                      NLSErrors := True;
                    end
                  else
                    begin
                      i := 0;
                      while i <= M - 1 do
                        begin
                          NLSErrors := NLSErrors or AP_FP_Greater(AbsReal(c[i] - c2[i]),
                            100 * NLThreshold);
                          inc(i);
                        end;
                    end;
                end;
              inc(pass);
            end;
          inc(M);
        end;

      //
      // test correctness of the RCond field
      //
      SetLength(a, n (* - 1 + 1 // optimized compiler *) , n (* - 1 + 1 // optimized compiler *) );
      SetLength(x, n (* - 1 + 1 // optimized compiler *) );
      SetLength(y, n (* - 1 + 1 // optimized compiler *) );
      SetLength(w, n (* - 1 + 1 // optimized compiler *) );
      v1 := MaxRealNumber;
      v2 := MinRealNumber;
      i := 0;
      while i <= n - 1 do
        begin
          x[i] := 0.1 + 0.9 * RandomReal;
          y[i] := 0.1 + 0.9 * RandomReal;
          w[i] := 1;
          j := 0;
          while j <= n - 1 do
            begin
              if i = j then
                begin
                  a[i, i] := 0.1 + 0.9 * RandomReal;
                  v1 := Min(v1, a[i, i]);
                  v2 := Max(v2, a[i, i]);
                end
              else
                begin
                  a[i, j] := 0;
                end;
              inc(j);
            end;
          inc(i);
        end;
      LSFitLinearW(y, w, a, n, n, Info, c, Rep);
      if Info <= 0 then
        begin
          LLSErrors := True;
        end
      else
        begin
          LLSErrors := LLSErrors or AP_FP_Greater(AbsReal(Rep.TaskRCond - v1 / v2),
            Threshold);
        end;
      inc(n);
    end;

  //
  // Test constrained least squares
  //
  pass := 1;
  while pass <= PassCount do
    begin
      n := 1;
      while n <= MaxN do
        begin
          M := 1;
          while M <= MaxM do
            begin

              //
              // test for K<>0
              //
              k := 1;
              while k <= M - 1 do
                begin

                  //
                  // Prepare Chebyshev basis. Its condition number is very good.
                  // Prepare constraints (random numbers)
                  //
                  SetLength(a, n, M);
                  SetLength(x, n);
                  SetLength(y, n);
                  SetLength(w, n);
                  XScale := 0.9 + 0.1 * RandomReal;
                  i := 0;
                  while i <= n - 1 do
                    begin
                      if n = 1 then
                        begin
                          x[i] := 2 * RandomReal - 1;
                        end
                      else
                        begin
                          x[i] := XScale * (AP_Float(2 * i) / (n - 1) - 1);
                        end;
                      y[i] := 3 * x[i] + Exp(x[i]);
                      w[i] := 1 + RandomReal;
                      a[i, 0] := 1;
                      if M > 1 then
                        begin
                          a[i, 1] := x[i];
                        end;
                      j := 2;
                      while j <= M - 1 do
                        begin
                          a[i, j] := 2 * x[i] * a[i, j - 1] - a[i, j - 2];
                          inc(j);
                        end;
                      inc(i);
                    end;
                  SetLength(CM, k, M + 1);
                  i := 0;
                  while i <= k - 1 do
                    begin
                      j := 0;
                      while j <= M do
                        begin
                          CM[i, j] := 2 * RandomReal - 1;
                          inc(j);
                        end;
                      inc(i);
                    end;

                  //
                  // Solve constrained task
                  //
                  LSFitLinearWC(y, w, a, CM, n, M, k, Info, c, Rep);
                  if Info <= 0 then
                    begin
                      LLSErrors := True;
                    end
                  else
                    begin
                      LLSErrors := LLSErrors or not IsGLSSolution(n, M, k, y, w,
                        a, CM, c);
                    end;

                  //
                  // test non-weighted fitting
                  //
                  SetLength(W2, n);
                  i := 0;
                  while i <= n - 1 do
                    begin
                      W2[i] := 1;
                      inc(i);
                    end;
                  LSFitLinearWC(y, W2, a, CM, n, M, k, Info, c, Rep);
                  LSFitLinearC(y, a, CM, n, M, k, Info2, c2, Rep2);
                  if (Info <= 0) or (Info2 <= 0) then
                    begin
                      LLSErrors := True;
                    end
                  else
                    begin

                      //
                      // test answer correctness
                      //
                      j := 0;
                      while j <= M - 1 do
                        begin
                          LLSErrors := LLSErrors or AP_FP_Greater(AbsReal(c[j] - c2[j]),
                            Threshold);
                          inc(j);
                        end;
                      LLSErrors := LLSErrors or
                        AP_FP_Greater(AbsReal(Rep.TaskRCond - Rep2.TaskRCond), Threshold);
                    end;
                  inc(k);
                end;
              inc(M);
            end;
          inc(n);
        end;
      inc(pass);
    end;

  //
  // nonlinear task for nonlinear fitting:
  //
  // f(X,C) = 1/(1+C*X^2),
  // C(true) = 2.
  //
  n := 100;
  SetLength(c, 1);
  c[0] := 1 + 2 * RandomReal;
  SetLength(a, n, 1);
  SetLength(y, n);
  i := 0;
  while i <= n - 1 do
    begin
      a[i, 0] := 4 * RandomReal - 2;
      y[i] := 1 / (1 + 2 * AP_Sqr(a[i, 0]));
      inc(i);
    end;
  LSFitNonlinearFG(a, y, c, n, 1, 1, True, State);
  LSFitNonlinearSetCond(State, 0.0, NLThreshold, 0);
  while LSFitNonlinearIteration(State) do
    begin
      if State.NeedF then
        begin
          State.f := 1 / (1 + State.c[0] * AP_Sqr(State.x[0]));
        end;
      if State.NeedFG then
        begin
          State.f := 1 / (1 + State.c[0] * AP_Sqr(State.x[0]));
          State.g[0] := -AP_Sqr(State.x[0]) /
            AP_Sqr(1 + State.c[0] * AP_Sqr(State.x[0]));
        end;
    end;
  LSFitNonlinearResults(State, Info, c, Rep);
  if Info <= 0 then
    begin
      NLSErrors := True;
    end
  else
    begin
      NLSErrors := NLSErrors or AP_FP_Greater(AbsReal(c[0] - 2),
        100 * NLThreshold);
    end;

  //
  // solve simple task (fitting by constant function) and check
  // correctness of the errors calculated by subroutines
  //
  pass := 1;
  while pass <= PassCount do
    begin

      //
      // test on task with non-zero Yi
      //
      n := 4;
      v1 := RandomReal;
      v2 := RandomReal;
      v := 1 + RandomReal;
      SetLength(c, 1);
      c[0] := 1 + 2 * RandomReal;
      SetLength(a, 4, 1);
      SetLength(y, 4);
      a[0, 0] := 1;
      y[0] := v - v2;
      a[1, 0] := 1;
      y[1] := v - v1;
      a[2, 0] := 1;
      y[2] := v + v1;
      a[3, 0] := 1;
      y[3] := v + v2;
      RefRMS := Sqrt((AP_Sqr(v1) + AP_Sqr(v2)) / 2);
      RefAvg := (AbsReal(v1) + AbsReal(v2)) / 2;
      RefAvgRel := 0.25 * (AbsReal(v2) / AbsReal(v - v2) + AbsReal(v1) /
        AbsReal(v - v1) + AbsReal(v1) / AbsReal(v + v1) + AbsReal(v2) /
        AbsReal(v + v2));
      RefMax := Max(v1, v2);

      //
      // Test LLS
      //
      LSFitLinear(y, a, 4, 1, Info, c, Rep);
      if Info <= 0 then
        begin
          LLSErrors := True;
        end
      else
        begin
          LLSErrors := LLSErrors or AP_FP_Greater(AbsReal(c[0] - v), Threshold);
          LLSErrors := LLSErrors or AP_FP_Greater(AbsReal(Rep.RMSError - RefRMS),
            Threshold);
          LLSErrors := LLSErrors or AP_FP_Greater(AbsReal(Rep.AvgError - RefAvg),
            Threshold);
          LLSErrors := LLSErrors or
            AP_FP_Greater(AbsReal(Rep.AvgRelError - RefAvgRel), Threshold);
          LLSErrors := LLSErrors or AP_FP_Greater(AbsReal(Rep.MaxError - RefMax),
            Threshold);
        end;

      //
      // Test NLS
      //
      LSFitNonlinearFG(a, y, c, 4, 1, 1, True, State);
      LSFitNonlinearSetCond(State, 0.0, NLThreshold, 0);
      while LSFitNonlinearIteration(State) do
        begin
          if State.NeedF then
            begin
              State.f := State.c[0];
            end;
          if State.NeedFG then
            begin
              State.f := State.c[0];
              State.g[0] := 1;
            end;
        end;
      LSFitNonlinearResults(State, Info, c, Rep);
      if Info <= 0 then
        begin
          NLSErrors := True;
        end
      else
        begin
          NLSErrors := NLSErrors or AP_FP_Greater(AbsReal(c[0] - v), Threshold);
          NLSErrors := NLSErrors or AP_FP_Greater(AbsReal(Rep.RMSError - RefRMS),
            Threshold);
          NLSErrors := NLSErrors or AP_FP_Greater(AbsReal(Rep.AvgError - RefAvg),
            Threshold);
          NLSErrors := NLSErrors or
            AP_FP_Greater(AbsReal(Rep.AvgRelError - RefAvgRel), Threshold);
          NLSErrors := NLSErrors or AP_FP_Greater(AbsReal(Rep.MaxError - RefMax),
            Threshold);
        end;
      inc(pass);
    end;

  //
  // report
  //
  WasErrors := LLSErrors or NLSErrors;
  if not Silent then
    begin
      DoStatusNoLn(PFormat('TESTING LEAST SQUARES'#13#10'', []));
      DoStatusNoLn(PFormat('LINEAR LEAST SQUARES:                    ', []));
      if LLSErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('NON-LINEAR LEAST SQUARES:                ', []));
      if NLSErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      if WasErrors then
        begin
          DoStatusNoLn(PFormat('TEST FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('TEST PASSED'#13#10'', []));
        end;
      DoStatusNoLn(PFormat(''#13#10''#13#10'', []));
    end;

  //
  // end
  //
  Result := not WasErrors;
end;

function TestPolInt(Silent: Boolean): Boolean;

  function InternalPolInt(const x: TLVec; f: TLVec;
    n: TLInt; t: TLFloat): TLFloat;
  var
    i: TLInt;
    j: TLInt;
  begin
    f := DynamicArrayCopy(f);
    n := n - 1;
    j := 0;
    while j <= n - 1 do
      begin
        i := j + 1;
        while i <= n do
          begin
            f[i] := ((t - x[j]) * f[i] - (t - x[i]) * f[j]) / (x[i] - x[j]);
            inc(i);
          end;
        inc(j);
      end;
    Result := f[n];
  end;

  procedure BRCUnset(var b: TBarycentricInterpolant);
  var
    x: TLVec;
    y: TLVec;
    w: TLVec;
  begin
    SetLength(x, 1);
    SetLength(y, 1);
    SetLength(w, 1);
    x[0] := 0;
    y[0] := 0;
    w[0] := 1;
    BarycentricBuildXYW(x, y, w, 1, b);
  end;

var
  WasErrors: Boolean;
  IntErrors: Boolean;
  FitErrors: Boolean;
  Threshold: TLFloat;
  x: TLVec;
  y: TLVec;
  w: TLVec;
  x2: TLVec;
  y2: TLVec;
  W2: TLVec;
  XFull: TLVec;
  YFull: TLVec;
  a: TLFloat;
  b: TLFloat;
  t: TLFloat;
  i: TLInt;
  k: TLInt;
  XC: TLVec;
  yc: TLVec;
  DC: TLIVec;
  Info: TLInt;
  Info2: TLInt;
  v: TLFloat;
  v0: TLFloat;
  v1: TLFloat;
  v2: TLFloat;
  s: TLFloat;
  XMin: TLFloat;
  XMax: TLFloat;
  RefRMS: TLFloat;
  RefAvg: TLFloat;
  RefAvgRel: TLFloat;
  RefMax: TLFloat;
  p: TBarycentricInterpolant;
  p1: TBarycentricInterpolant;
  p2: TBarycentricInterpolant;
  Rep: TPolynomialFitReport;
  Rep2: TPolynomialFitReport;
  n: TLInt;
  M: TLInt;
  MaxN: TLInt;
  pass: TLInt;
  PassCount: TLInt;
begin
  WasErrors := False;
  IntErrors := False;
  FitErrors := False;
  MaxN := 5;
  PassCount := 20;
  Threshold := 1.0E8 * MachineEpsilon;

  //
  // Test equidistant interpolation
  //
  pass := 1;
  while pass <= PassCount do
    begin
      n := 1;
      while n <= MaxN do
        begin

          //
          // prepare task:
          // * equidistant points
          // * random Y
          // * T in [A,B] or near (within 10% of its width)
          //
          repeat
            a := 2 * RandomReal - 1;
            b := 2 * RandomReal - 1;
          until AP_FP_Greater(AbsReal(a - b), 0.2);
          t := a + (1.2 * RandomReal - 0.1) * (b - a);
          TaskGenInt1DEquidist(a, b, n, x, y);

          //
          // test "fast" equidistant interpolation (no barycentric model)
          //
          IntErrors := IntErrors or
            AP_FP_Greater(AbsReal(PolynomialCalcEqDist(a, b, y, n,
            t) - InternalPolInt(x, y, n, t)), Threshold);

          //
          // test "slow" equidistant interpolation (create barycentric model)
          //
          BRCUnset(p);
          PolynomialBuild(x, y, n, p);
          IntErrors := IntErrors or
            AP_FP_Greater(AbsReal(BarycentricCalc(p, t) - InternalPolInt(x, y, n, t)
            ), Threshold);

          //
          // test "fast" interpolation (create "fast" barycentric model)
          //
          BRCUnset(p);
          PolynomialBuildEqDist(a, b, y, n, p);
          IntErrors := IntErrors or
            AP_FP_Greater(AbsReal(BarycentricCalc(p, t) - InternalPolInt(x, y, n, t)
            ), Threshold);
          inc(n);
        end;
      inc(pass);
    end;

  //
  // Test Chebyshev-1 interpolation
  //
  pass := 1;
  while pass <= PassCount do
    begin
      n := 1;
      while n <= MaxN do
        begin

          //
          // prepare task:
          // * equidistant points
          // * random Y
          // * T in [A,B] or near (within 10% of its width)
          //
          repeat
            a := 2 * RandomReal - 1;
            b := 2 * RandomReal - 1;
          until AP_FP_Greater(AbsReal(a - b), 0.2);
          t := a + (1.2 * RandomReal - 0.1) * (b - a);
          TaskGenInt1DCheb1(a, b, n, x, y);

          //
          // test "fast" interpolation (no barycentric model)
          //
          IntErrors := IntErrors or
            AP_FP_Greater(AbsReal(PolynomialCalcCheb1(a, b, y, n,
            t) - InternalPolInt(x, y, n, t)), Threshold);

          //
          // test "slow" interpolation (create barycentric model)
          //
          BRCUnset(p);
          PolynomialBuild(x, y, n, p);
          IntErrors := IntErrors or
            AP_FP_Greater(AbsReal(BarycentricCalc(p, t) - InternalPolInt(x, y, n, t)
            ), Threshold);

          //
          // test "fast" interpolation (create "fast" barycentric model)
          //
          BRCUnset(p);
          PolynomialBuildCheb1(a, b, y, n, p);
          IntErrors := IntErrors or
            AP_FP_Greater(AbsReal(BarycentricCalc(p, t) - InternalPolInt(x, y, n, t)
            ), Threshold);
          inc(n);
        end;
      inc(pass);
    end;

  //
  // Test Chebyshev-2 interpolation
  //
  pass := 1;
  while pass <= PassCount do
    begin
      n := 1;
      while n <= MaxN do
        begin

          //
          // prepare task:
          // * equidistant points
          // * random Y
          // * T in [A,B] or near (within 10% of its width)
          //
          repeat
            a := 2 * RandomReal - 1;
            b := 2 * RandomReal - 1;
          until AP_FP_Greater(AbsReal(a - b), 0.2);
          t := a + (1.2 * RandomReal - 0.1) * (b - a);
          TaskGenInt1DCheb2(a, b, n, x, y);

          //
          // test "fast" interpolation (no barycentric model)
          //
          IntErrors := IntErrors or
            AP_FP_Greater(AbsReal(PolynomialCalcCheb2(a, b, y, n,
            t) - InternalPolInt(x, y, n, t)), Threshold);

          //
          // test "slow" interpolation (create barycentric model)
          //
          BRCUnset(p);
          PolynomialBuild(x, y, n, p);
          IntErrors := IntErrors or
            AP_FP_Greater(AbsReal(BarycentricCalc(p, t) - InternalPolInt(x, y, n, t)
            ), Threshold);

          //
          // test "fast" interpolation (create "fast" barycentric model)
          //
          BRCUnset(p);
          PolynomialBuildCheb2(a, b, y, n, p);
          IntErrors := IntErrors or
            AP_FP_Greater(AbsReal(BarycentricCalc(p, t) - InternalPolInt(x, y, n, t)
            ), Threshold);
          inc(n);
        end;
      inc(pass);
    end;

  //
  // crash-test: ability to solve tasks which will overflow/underflow
  // weights with straightforward implementation

  //
  n := 1;
  while n <= 20 do
    begin
      a := -0.1 * MaxRealNumber;
      b := +0.1 * MaxRealNumber;
      TaskGenInt1DEquidist(a, b, n, x, y);
      PolynomialBuild(x, y, n, p);
      i := 0;
      while i <= n - 1 do
        begin
          IntErrors := IntErrors or AP_FP_Eq(p.w[i], 0);
          inc(i);
        end;
      inc(n);
    end;

  //
  // Test rational fitting:
  //
  pass := 1;
  while pass <= PassCount do
    begin
      n := 1;
      while n <= MaxN do
        begin

          //
          // N=M+K fitting (i.e. interpolation)
          //
          k := 0;
          while k <= n - 1 do
            begin
              TaskGenInt1D(-1, 1, n, XFull, YFull);
              SetLength(x, n - k);
              SetLength(y, n - k);
              SetLength(w, n - k);
              if k > 0 then
                begin
                  SetLength(XC, k);
                  SetLength(yc, k);
                  SetLength(DC, k);
                end;
              i := 0;
              while i <= n - k - 1 do
                begin
                  x[i] := XFull[i];
                  y[i] := YFull[i];
                  w[i] := 1 + RandomReal;
                  inc(i);
                end;
              i := 0;
              while i <= k - 1 do
                begin
                  XC[i] := XFull[n - k + i];
                  yc[i] := YFull[n - k + i];
                  DC[i] := 0;
                  inc(i);
                end;
              PolynomialFitWC(x, y, w, n - k, XC, yc, DC, k, n, Info, p1, Rep);
              if Info <= 0 then
                begin
                  FitErrors := True;
                end
              else
                begin
                  i := 0;
                  while i <= n - k - 1 do
                    begin
                      FitErrors := FitErrors or
                        AP_FP_Greater(AbsReal(BarycentricCalc(p1, x[i]) - y[i]),
                        Threshold);
                      inc(i);
                    end;
                  i := 0;
                  while i <= k - 1 do
                    begin
                      FitErrors := FitErrors or
                        AP_FP_Greater(AbsReal(BarycentricCalc(p1, XC[i]) - yc[i]),
                        Threshold);
                      inc(i);
                    end;
                end;
              inc(k);
            end;

          //
          // Testing constraints on derivatives.
          // Special tasks which will always have solution:
          // 1. P(0)=YC[0]
          // 2. P(0)=YC[0], P'(0)=YC[1]
          //
          if n > 1 then
            begin
              M := 3;
              while M <= 5 do
                begin
                  k := 1;
                  while k <= 2 do
                    begin
                      TaskGenInt1D(-1, 1, n, x, y);
                      SetLength(w, n);
                      SetLength(XC, 2);
                      SetLength(yc, 2);
                      SetLength(DC, 2);
                      i := 0;
                      while i <= n - 1 do
                        begin
                          w[i] := 1 + RandomReal;
                          inc(i);
                        end;
                      XC[0] := 0;
                      yc[0] := 2 * RandomReal - 1;
                      DC[0] := 0;
                      XC[1] := 0;
                      yc[1] := 2 * RandomReal - 1;
                      DC[1] := 1;
                      PolynomialFitWC(x, y, w, n, XC, yc, DC, k, M, Info, p1, Rep);
                      if Info <= 0 then
                        begin
                          FitErrors := True;
                        end
                      else
                        begin
                          BarycentricDiff1(p1, 0.0, v0, v1);
                          FitErrors := FitErrors or AP_FP_Greater(AbsReal(v0 - yc[0]),
                            Threshold);
                          if k = 2 then
                            begin
                              FitErrors := FitErrors or AP_FP_Greater(AbsReal(v1 - yc[1]),
                                Threshold);
                            end;
                        end;
                      inc(k);
                    end;
                  inc(M);
                end;
            end;
          inc(n);
        end;
      inc(pass);
    end;
  M := 2;
  while M <= 8 do
    begin
      pass := 1;
      while pass <= PassCount do
        begin

          //
          // General fitting
          //
          // interpolating function through M nodes should have
          // greater RMS error than fitting it through the same M nodes
          //
          n := 100;
          SetLength(x2, n);
          SetLength(y2, n);
          SetLength(W2, n);
          XMin := 0;
          XMax := 2 * pi;
          i := 0;
          while i <= n - 1 do
            begin
              x2[i] := 2 * pi * RandomReal;
              y2[i] := Sin(x2[i]);
              W2[i] := 1;
              inc(i);
            end;
          SetLength(x, M);
          SetLength(y, M);
          i := 0;
          while i <= M - 1 do
            begin
              x[i] := XMin + (XMax - XMin) * i / (M - 1);
              y[i] := Sin(x[i]);
              inc(i);
            end;
          PolynomialBuild(x, y, M, p1);
          PolynomialFitWC(x2, y2, W2, n, XC, yc, DC, 0, M, Info, p2, Rep);
          if Info <= 0 then
            begin
              FitErrors := True;
            end
          else
            begin

              //
              // calculate P1 (interpolant) RMS error, compare with P2 error
              //
              v1 := 0;
              v2 := 0;
              i := 0;
              while i <= n - 1 do
                begin
                  v1 := v1 + AP_Sqr(BarycentricCalc(p1, x2[i]) - y2[i]);
                  v2 := v2 + AP_Sqr(BarycentricCalc(p2, x2[i]) - y2[i]);
                  inc(i);
                end;
              v1 := Sqrt(v1 / n);
              v2 := Sqrt(v2 / n);
              FitErrors := FitErrors or AP_FP_Greater(v2, v1);
              FitErrors := FitErrors or AP_FP_Greater(AbsReal(v2 - Rep.RMSError),
                Threshold);
            end;

          //
          // compare weighted and non-weighted
          //
          n := 20;
          SetLength(x, n);
          SetLength(y, n);
          SetLength(w, n);
          i := 0;
          while i <= n - 1 do
            begin
              x[i] := 2 * RandomReal - 1;
              y[i] := 2 * RandomReal - 1;
              w[i] := 1;
              inc(i);
            end;
          PolynomialFitWC(x, y, w, n, XC, yc, DC, 0, M, Info, p1, Rep);
          PolynomialFit(x, y, n, M, Info2, p2, Rep2);
          if (Info <= 0) or (Info2 <= 0) then
            begin
              FitErrors := True;
            end
          else
            begin

              //
              // calculate P1 (interpolant), compare with P2 error
              // compare RMS errors
              //
              t := 2 * RandomReal - 1;
              v1 := BarycentricCalc(p1, t);
              v2 := BarycentricCalc(p2, t);
              FitErrors := FitErrors or AP_FP_NEq(v2, v1);
              FitErrors := FitErrors or AP_FP_NEq(Rep.RMSError, Rep2.RMSError);
              FitErrors := FitErrors or AP_FP_NEq(Rep.AvgError, Rep2.AvgError);
              FitErrors := FitErrors or AP_FP_NEq(Rep.AvgRelError, Rep2.AvgRelError);
              FitErrors := FitErrors or AP_FP_NEq(Rep.MaxError, Rep2.MaxError);
            end;
          inc(pass);
        end;
      inc(M);
    end;
  pass := 1;
  while pass <= PassCount do
    begin
      Assert(PassCount >= 2, 'PassCount should be 2 or greater!');

      //
      // solve simple task (all X[] are the same, Y[] are specially
      // calculated to ensure simple form of all types of errors)
      // and check correctness of the errors calculated by subroutines
      //
      // First pass is done with zero Y[], other passes - with random Y[].
      // It should test both ability to correctly calculate errors and
      // ability to not fail while working with zeros :)
      //
      n := 4;
      if pass = 1 then
        begin
          v1 := 0;
          v2 := 0;
          v := 0;
        end
      else
        begin
          v1 := RandomReal;
          v2 := RandomReal;
          v := 1 + RandomReal;
        end;
      SetLength(x, 4);
      SetLength(y, 4);
      SetLength(w, 4);
      x[0] := 0;
      y[0] := v - v2;
      w[0] := 1;
      x[1] := 0;
      y[1] := v - v1;
      w[1] := 1;
      x[2] := 0;
      y[2] := v + v1;
      w[2] := 1;
      x[3] := 0;
      y[3] := v + v2;
      w[3] := 1;
      RefRMS := Sqrt((AP_Sqr(v1) + AP_Sqr(v2)) / 2);
      RefAvg := (AbsReal(v1) + AbsReal(v2)) / 2;
      if pass = 1 then
        begin
          RefAvgRel := 0;
        end
      else
        begin
          RefAvgRel := 0.25 * (AbsReal(v2) / AbsReal(v - v2) + AbsReal(v1) /
            AbsReal(v - v1) + AbsReal(v1) / AbsReal(v + v1) + AbsReal(v2) /
            AbsReal(v + v2));
        end;
      RefMax := Max(v1, v2);

      //
      // Test errors correctness
      //
      PolynomialFit(x, y, 4, 1, Info, p, Rep);
      if Info <= 0 then
        begin
          FitErrors := True;
        end
      else
        begin
          s := BarycentricCalc(p, 0);
          FitErrors := FitErrors or AP_FP_Greater(AbsReal(s - v), Threshold);
          FitErrors := FitErrors or AP_FP_Greater(AbsReal(Rep.RMSError - RefRMS),
            Threshold);
          FitErrors := FitErrors or AP_FP_Greater(AbsReal(Rep.AvgError - RefAvg),
            Threshold);
          FitErrors := FitErrors or
            AP_FP_Greater(AbsReal(Rep.AvgRelError - RefAvgRel), Threshold);
          FitErrors := FitErrors or AP_FP_Greater(AbsReal(Rep.MaxError - RefMax),
            Threshold);
        end;
      inc(pass);
    end;

  //
  // report
  //
  WasErrors := IntErrors or FitErrors;
  if not Silent then
    begin
      DoStatusNoLn(PFormat('TESTING POLYNOMIAL INTERPOLATION AND FITTING'#13#10'', []));

      //
      // Normal tests
      //
      DoStatusNoLn(PFormat('INTERPOLATION TEST:                      ', []));
      if IntErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('FITTING TEST:                            ', []));
      if FitErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      if WasErrors then
        begin
          DoStatusNoLn(PFormat('TEST FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('TEST PASSED'#13#10'', []));
        end;
      DoStatusNoLn(PFormat(''#13#10''#13#10'', []));
    end;

  //
  // end
  //
  Result := not WasErrors;
end;

function TestSplineInterpolation(Silent: Boolean): Boolean;

(* ************************************************************************
  Lipschitz constants for spline inself, first and second derivatives.
  ************************************************************************ *)
  procedure LConst(a: TLFloat; b: TLFloat; const c: TSpline1DInterpolant;
    LStep: TLFloat; var L0: TLFloat; var L1: TLFloat;
    var l2: TLFloat);
  var
    t: TLFloat;
    vl: TLFloat;
    VM: TLFloat;
    vr: TLFloat;
    PrevF: TLFloat;
    PrevD: TLFloat;
    PrevD2: TLFloat;
    f: TLFloat;
    d: TLFloat;
    d2: TLFloat;
  begin
    L0 := 0;
    L1 := 0;
    l2 := 0;
    t := a - 0.1;
    vl := Spline1DCalc(c, t - 2 * LStep);
    VM := Spline1DCalc(c, t - LStep);
    vr := Spline1DCalc(c, t);
    f := VM;
    d := (vr - vl) / (2 * LStep);
    d2 := (vr - 2 * VM + vl) / AP_Sqr(LStep);
    while AP_FP_Less_Eq(t, b + 0.1) do
      begin
        PrevF := f;
        PrevD := d;
        PrevD2 := d2;
        vl := VM;
        VM := vr;
        vr := Spline1DCalc(c, t + LStep);
        f := VM;
        d := (vr - vl) / (2 * LStep);
        d2 := (vr - 2 * VM + vl) / AP_Sqr(LStep);
        L0 := Max(L0, AbsReal((f - PrevF) / LStep));
        L1 := Max(L1, AbsReal((d - PrevD) / LStep));
        l2 := Max(l2, AbsReal((d2 - PrevD2) / LStep));
        t := t + LStep;
      end;
  end;

(* ************************************************************************
  Unpack testing
  ************************************************************************ *)
  function TestUnpack(const c: TSpline1DInterpolant;
    const x: TLVec): Boolean;
  var
    i: TLInt;
    n: TLInt;
    Err: TLFloat;
    t: TLFloat;
    v1: TLFloat;
    v2: TLFloat;
    pass: TLInt;
    PassCount: TLInt;
    Tbl: TLMatrix;
  begin
    PassCount := 20;
    Err := 0;
    Spline1DUnpack(c, n, Tbl);
    i := 0;
    while i <= n - 2 do
      begin
        pass := 1;
        while pass <= PassCount do
          begin
            t := RandomReal * (Tbl[i, 1] - Tbl[i, 0]);
            v1 := Tbl[i, 2] + t * Tbl[i, 3] + AP_Sqr(t) * Tbl[i, 4] + t * AP_Sqr(t)
              * Tbl[i, 5];
            v2 := Spline1DCalc(c, Tbl[i, 0] + t);
            Err := Max(Err, AbsReal(v1 - v2));
            inc(pass);
          end;
        inc(i);
      end;
    i := 0;
    while i <= n - 2 do
      begin
        Err := Max(Err, AbsReal(x[i] - Tbl[i, 0]));
        inc(i);
      end;
    i := 0;
    while i <= n - 2 do
      begin
        Err := Max(Err, AbsReal(x[i + 1] - Tbl[i, 1]));
        inc(i);
      end;
    Result := AP_FP_Less(Err, 100 * MachineEpsilon);
  end;

(* ************************************************************************
  Unset spline, i.e. initialize it with random garbage
  ************************************************************************ *)
  procedure UnsetSpline1D(var c: TSpline1DInterpolant);
  var
    x: TLVec;
    y: TLVec;
    d: TLVec;
  begin
    SetLength(x, 2);
    SetLength(y, 2);
    SetLength(d, 2);
    x[0] := -1;
    y[0] := RandomReal;
    d[0] := RandomReal;
    x[1] := 1;
    y[1] := RandomReal;
    d[1] := RandomReal;
    Spline1DBuildHermite(x, y, d, 2, c);
  end;

(* ************************************************************************
  Unsets real vector
  ************************************************************************ *)
  procedure Unset1D(var x: TLVec);
  begin
    SetLength(x, 1);
    x[0] := 2 * RandomReal - 1;
  end;

(* ************************************************************************
  Tests whether constant C is solution of 1D LLS problem
  ************************************************************************ *)
  function Is1DSolution(n: TLInt; const y: TLVec;
    const w: TLVec; c: TLFloat): Boolean;
  var
    i: TLInt;
    v: TLFloat;
    s1: TLFloat;
    s2: TLFloat;
    s3: TLFloat;
    Delta: TLFloat;
  begin
    Delta := 0.001;

    //
    // Test result
    //
    s1 := 0;
    i := 0;
    while i <= n - 1 do
      begin
        s1 := s1 + AP_Sqr(w[i] * (c - y[i]));
        inc(i);
      end;
    s2 := 0;
    s3 := 0;
    i := 0;
    while i <= n - 1 do
      begin
        s2 := s2 + AP_Sqr(w[i] * (c + Delta - y[i]));
        s3 := s3 + AP_Sqr(w[i] * (c - Delta - y[i]));
        inc(i);
      end;
    Result := AP_FP_Greater_Eq(s2, s1) and AP_FP_Greater_Eq(s3, s1);
  end;

var
  WasErrors: Boolean;
  CRSErrors: Boolean;
  CSErrors: Boolean;
  HSErrors: Boolean;
  ASErrors: Boolean;
  LSErrors: Boolean;
  DSErrors: Boolean;
  UPErrors: Boolean;
  CPErrors: Boolean;
  LTErrors: Boolean;
  IErrors: Boolean;
  FitErrors: Boolean;
  NonStrictThreshold: TLFloat;
  Threshold: TLFloat;
  PassCount: TLInt;
  LStep: TLFloat;
  h: TLFloat;
  MaxN: TLInt;
  BLType: TLInt;
  BRType: TLInt;
  PeriodicCond: Boolean;
  n: TLInt;
  M: TLInt;
  i: TLInt;
  k: TLInt;
  pass: TLInt;
  SType: TLInt;
  x: TLVec;
  y: TLVec;
  yp: TLVec;
  w: TLVec;
  W2: TLVec;
  y2: TLVec;
  d: TLVec;
  XC: TLVec;
  yc: TLVec;
  DC: TLIVec;
  c: TSpline1DInterpolant;
  c2: TSpline1DInterpolant;
  Info: TLInt;
  Info1: TLInt;
  Info2: TLInt;
  a: TLFloat;
  b: TLFloat;
  BL: TLFloat;
  BR: TLFloat;
  t: TLFloat;
  SA: TLFloat;
  SB: TLFloat;
  v: TLFloat;
  v1: TLFloat;
  v2: TLFloat;
  L10: TLFloat;
  L11: TLFloat;
  L12: TLFloat;
  L20: TLFloat;
  L21: TLFloat;
  L22: TLFloat;
  P0: TLFloat;
  p1: TLFloat;
  p2: TLFloat;
  s: TLFloat;
  DS: TLFloat;
  D2S: TLFloat;
  s2: TLFloat;
  DS2: TLFloat;
  D2S2: TLFloat;
  vl: TLFloat;
  VM: TLFloat;
  vr: TLFloat;
  Err: TLFloat;
  Tension: TLFloat;
  IntAB: TLFloat;
  Rep: TSpline1DFitReport;
  Rep2: TSpline1DFitReport;
  RefRMS: TLFloat;
  RefAvg: TLFloat;
  RefAvgRel: TLFloat;
  RefMax: TLFloat;
begin
  WasErrors := False;
  PassCount := 20;
  LStep := 0.005;
  h := 0.00001;
  MaxN := 10;
  Threshold := 10000 * MachineEpsilon;
  NonStrictThreshold := 0.00001;
  LSErrors := False;
  CSErrors := False;
  CRSErrors := False;
  HSErrors := False;
  ASErrors := False;
  DSErrors := False;
  CPErrors := False;
  UPErrors := False;
  LTErrors := False;
  IErrors := False;
  FitErrors := False;

  //
  // General test: linear, cubic, Hermite, Akima
  //
  n := 2;
  while n <= MaxN do
    begin
      SetLength(x, n (* - 1 + 1 // optimized compiler *) );
      SetLength(y, n (* - 1 + 1 // optimized compiler *) );
      SetLength(yp, n (* - 1 + 1 // optimized compiler *) );
      SetLength(d, n (* - 1 + 1 // optimized compiler *) );
      pass := 1;
      while pass <= PassCount do
        begin

          //
          // Prepare task:
          // * X contains abscissas from [A,B]
          // * Y contains function values
          // * YP contains periodic function values
          //
          a := -1 - RandomReal;
          b := +1 + RandomReal;
          BL := 2 * RandomReal - 1;
          BR := 2 * RandomReal - 1;
          i := 0;
          while i <= n - 1 do
            begin
              x[i] := 0.5 * (b + a) + 0.5 * (b - a) * Cos(pi * (2 * i + 1) / (2 * n));
              if i = 0 then
                begin
                  x[i] := a;
                end;
              if i = n - 1 then
                begin
                  x[i] := b;
                end;
              y[i] := Cos(1.3 * pi * x[i] + 0.4);
              yp[i] := y[i];
              d[i] := -1.3 * pi * Sin(1.3 * pi * x[i] + 0.4);
              inc(i);
            end;
          yp[n - 1] := yp[0];
          i := 0;
          while i <= n - 1 do
            begin
              k := RandomInteger(n);
              if k <> i then
                begin
                  t := x[i];
                  x[i] := x[k];
                  x[k] := t;
                  t := y[i];
                  y[i] := y[k];
                  y[k] := t;
                  t := yp[i];
                  yp[i] := yp[k];
                  yp[k] := t;
                  t := d[i];
                  d[i] := d[k];
                  d[k] := t;
                end;
              inc(i);
            end;

          //
          // Build linear spline
          // Test for general interpolation scheme properties:
          // * values at nodes
          // * continuous function
          // Test for specific properties is implemented below.
          //
          Spline1DBuildLinear(x, y, n, c);
          Err := 0;
          i := 0;
          while i <= n - 1 do
            begin
              Err := Max(Err, AbsReal(y[i] - Spline1DCalc(c, x[i])));
              inc(i);
            end;
          LSErrors := LSErrors or AP_FP_Greater(Err, Threshold);
          LConst(a, b, c, LStep, L10, L11, L12);
          LConst(a, b, c, LStep / 3, L20, L21, L22);
          LSErrors := LSErrors or AP_FP_Greater(L20 / L10, 1.2);

          //
          // Build cubic spline.
          // Test for interpolation scheme properties:
          // * values at nodes
          // * boundary conditions
          // * continuous function
          // * continuous first derivative
          // * continuous second derivative
          // * periodicity properties
          //
          BLType := -1;
          while BLType <= 2 do
            begin
              BRType := -1;
              while BRType <= 2 do
                begin

                  //
                  // skip meaningless combination of boundary conditions
                  // (one condition is periodic, another is not)
                  //
                  PeriodicCond := (BLType = -1) or (BRType = -1);
                  if PeriodicCond and (BLType <> BRType) then
                    begin
                      inc(BRType);
                      Continue;
                    end;

                  //
                  // build
                  //
                  if PeriodicCond then
                    begin
                      Spline1DBuildCubic(x, yp, n, BLType, BL, BRType, BR, c);
                    end
                  else
                    begin
                      Spline1DBuildCubic(x, y, n, BLType, BL, BRType, BR, c);
                    end;

                  //
                  // interpolation properties
                  //
                  Err := 0;
                  if PeriodicCond then
                    begin

                      //
                      // * check values at nodes; spline is periodic so
                      // we add random number of periods to nodes
                      // * we also test for periodicity of derivatives
                      //
                      i := 0;
                      while i <= n - 1 do
                        begin
                          v := x[i];
                          VM := v + (b - a) * (RandomInteger(5) - 2);
                          t := yp[i] - Spline1DCalc(c, VM);
                          Err := Max(Err, AbsReal(t));
                          Spline1DDiff(c, v, s, DS, D2S);
                          Spline1DDiff(c, VM, s2, DS2, D2S2);
                          Err := Max(Err, AbsReal(s - s2));
                          Err := Max(Err, AbsReal(DS - DS2));
                          Err := Max(Err, AbsReal(D2S - D2S2));
                          inc(i);
                        end;

                      //
                      // periodicity between nodes
                      //
                      v := a + (b - a) * RandomReal;
                      VM := v + (b - a) * (RandomInteger(5) - 2);
                      Err := Max(Err, AbsReal(Spline1DCalc(c, v) - Spline1DCalc(c, VM)));
                      Spline1DDiff(c, v, s, DS, D2S);
                      Spline1DDiff(c, VM, s2, DS2, D2S2);
                      Err := Max(Err, AbsReal(s - s2));
                      Err := Max(Err, AbsReal(DS - DS2));
                      Err := Max(Err, AbsReal(D2S - D2S2));
                    end
                  else
                    begin

                      //
                      // * check values at nodes
                      //
                      i := 0;
                      while i <= n - 1 do
                        begin
                          Err := Max(Err, AbsReal(y[i] - Spline1DCalc(c, x[i])));
                          inc(i);
                        end;
                    end;
                  CSErrors := CSErrors or AP_FP_Greater(Err, Threshold);

                  //
                  // check boundary conditions
                  //
                  Err := 0;
                  if BLType = 0 then
                    begin
                      Spline1DDiff(c, a - h, s, DS, D2S);
                      Spline1DDiff(c, a + h, s2, DS2, D2S2);
                      t := (D2S2 - D2S) / (2 * h);
                      Err := Max(Err, AbsReal(t));
                    end;
                  if BLType = 1 then
                    begin
                      t := (Spline1DCalc(c, a + h) - Spline1DCalc(c, a - h)) / (2 * h);
                      Err := Max(Err, AbsReal(BL - t));
                    end;
                  if BLType = 2 then
                    begin
                      t := (Spline1DCalc(c, a + h) - 2 * Spline1DCalc(c, a) +
                        Spline1DCalc(c, a - h)) / AP_Sqr(h);
                      Err := Max(Err, AbsReal(BL - t));
                    end;
                  if BRType = 0 then
                    begin
                      Spline1DDiff(c, b - h, s, DS, D2S);
                      Spline1DDiff(c, b + h, s2, DS2, D2S2);
                      t := (D2S2 - D2S) / (2 * h);
                      Err := Max(Err, AbsReal(t));
                    end;
                  if BRType = 1 then
                    begin
                      t := (Spline1DCalc(c, b + h) - Spline1DCalc(c, b - h)) / (2 * h);
                      Err := Max(Err, AbsReal(BR - t));
                    end;
                  if BRType = 2 then
                    begin
                      t := (Spline1DCalc(c, b + h) - 2 * Spline1DCalc(c, b) +
                        Spline1DCalc(c, b - h)) / AP_Sqr(h);
                      Err := Max(Err, AbsReal(BR - t));
                    end;
                  if (BLType = -1) or (BRType = -1) then
                    begin
                      Spline1DDiff(c, a + 100 * MachineEpsilon, s, DS, D2S);
                      Spline1DDiff(c, b - 100 * MachineEpsilon, s2, DS2, D2S2);
                      Err := Max(Err, AbsReal(s - s2));
                      Err := Max(Err, AbsReal(DS - DS2));
                      Err := Max(Err, AbsReal(D2S - D2S2));
                    end;
                  CSErrors := CSErrors or AP_FP_Greater(Err, 1.0E-3);

                  //
                  // Check Lipschitz continuity
                  //
                  LConst(a, b, c, LStep, L10, L11, L12);
                  LConst(a, b, c, LStep / 3, L20, L21, L22);
                  if AP_FP_Greater(L10, 1.0E-6) then
                    begin
                      CSErrors := CSErrors or AP_FP_Greater(L20 / L10, 1.2);
                    end;
                  if AP_FP_Greater(L11, 1.0E-6) then
                    begin
                      CSErrors := CSErrors or AP_FP_Greater(L21 / L11, 1.2);
                    end;
                  if AP_FP_Greater(L12, 1.0E-6) then
                    begin
                      CSErrors := CSErrors or AP_FP_Greater(L22 / L12, 1.2);
                    end;
                  inc(BRType);
                end;
              inc(BLType);
            end;

          //
          // Build Catmull-Rom spline.
          // Test for interpolation scheme properties:
          // * values at nodes
          // * boundary conditions
          // * continuous function
          // * continuous first derivative
          // * periodicity properties
          //
          BLType := -1;
          while BLType <= 0 do
            begin
              PeriodicCond := BLType = -1;

              //
              // select random tension value, then build
              //
              if AP_FP_Greater(RandomReal, 0.5) then
                begin
                  if AP_FP_Greater(RandomReal, 0.5) then
                    begin
                      Tension := 0;
                    end
                  else
                    begin
                      Tension := 1;
                    end;
                end
              else
                begin
                  Tension := RandomReal;
                end;
              if PeriodicCond then
                begin
                  Spline1DBuildCatmullRom(x, yp, n, BLType, Tension, c);
                end
              else
                begin
                  Spline1DBuildCatmullRom(x, y, n, BLType, Tension, c);
                end;

              //
              // interpolation properties
              //
              Err := 0;
              if PeriodicCond then
                begin

                  //
                  // * check values at nodes; spline is periodic so
                  // we add random number of periods to nodes
                  // * we also test for periodicity of first derivative
                  //
                  i := 0;
                  while i <= n - 1 do
                    begin
                      v := x[i];
                      VM := v + (b - a) * (RandomInteger(5) - 2);
                      t := yp[i] - Spline1DCalc(c, VM);
                      Err := Max(Err, AbsReal(t));
                      Spline1DDiff(c, v, s, DS, D2S);
                      Spline1DDiff(c, VM, s2, DS2, D2S2);
                      Err := Max(Err, AbsReal(s - s2));
                      Err := Max(Err, AbsReal(DS - DS2));
                      inc(i);
                    end;

                  //
                  // periodicity between nodes
                  //
                  v := a + (b - a) * RandomReal;
                  VM := v + (b - a) * (RandomInteger(5) - 2);
                  Err := Max(Err, AbsReal(Spline1DCalc(c, v) - Spline1DCalc(c, VM)));
                  Spline1DDiff(c, v, s, DS, D2S);
                  Spline1DDiff(c, VM, s2, DS2, D2S2);
                  Err := Max(Err, AbsReal(s - s2));
                  Err := Max(Err, AbsReal(DS - DS2));
                end
              else
                begin

                  //
                  // * check values at nodes
                  //
                  i := 0;
                  while i <= n - 1 do
                    begin
                      Err := Max(Err, AbsReal(y[i] - Spline1DCalc(c, x[i])));
                      inc(i);
                    end;
                end;
              CRSErrors := CRSErrors or AP_FP_Greater(Err, Threshold);

              //
              // check boundary conditions
              //
              Err := 0;
              if BLType = 0 then
                begin
                  Spline1DDiff(c, a - h, s, DS, D2S);
                  Spline1DDiff(c, a + h, s2, DS2, D2S2);
                  t := (D2S2 - D2S) / (2 * h);
                  Err := Max(Err, AbsReal(t));
                  Spline1DDiff(c, b - h, s, DS, D2S);
                  Spline1DDiff(c, b + h, s2, DS2, D2S2);
                  t := (D2S2 - D2S) / (2 * h);
                  Err := Max(Err, AbsReal(t));
                end;
              if BLType = -1 then
                begin
                  Spline1DDiff(c, a + 100 * MachineEpsilon, s, DS, D2S);
                  Spline1DDiff(c, b - 100 * MachineEpsilon, s2, DS2, D2S2);
                  Err := Max(Err, AbsReal(s - s2));
                  Err := Max(Err, AbsReal(DS - DS2));
                end;
              CRSErrors := CRSErrors or AP_FP_Greater(Err, 1.0E-3);

              //
              // Check Lipschitz continuity
              //
              LConst(a, b, c, LStep, L10, L11, L12);
              LConst(a, b, c, LStep / 3, L20, L21, L22);
              if AP_FP_Greater(L10, 1.0E-6) then
                begin
                  CRSErrors := CRSErrors or AP_FP_Greater(L20 / L10, 1.2);
                end;
              if AP_FP_Greater(L11, 1.0E-6) then
                begin
                  CRSErrors := CRSErrors or AP_FP_Greater(L21 / L11, 1.2);
                end;
              inc(BLType);
            end;

          //
          // Build Hermite spline.
          // Test for interpolation scheme properties:
          // * values and derivatives at nodes
          // * continuous function
          // * continuous first derivative
          //
          Spline1DBuildHermite(x, y, d, n, c);
          Err := 0;
          i := 0;
          while i <= n - 1 do
            begin
              Err := Max(Err, AbsReal(y[i] - Spline1DCalc(c, x[i])));
              inc(i);
            end;
          HSErrors := HSErrors or AP_FP_Greater(Err, Threshold);
          Err := 0;
          i := 0;
          while i <= n - 1 do
            begin
              t := (Spline1DCalc(c, x[i] + h) - Spline1DCalc(c, x[i] - h)) / (2 * h);
              Err := Max(Err, AbsReal(d[i] - t));
              inc(i);
            end;
          HSErrors := HSErrors or AP_FP_Greater(Err, 1.0E-3);
          LConst(a, b, c, LStep, L10, L11, L12);
          LConst(a, b, c, LStep / 3, L20, L21, L22);
          HSErrors := HSErrors or AP_FP_Greater(L20 / L10, 1.2);
          HSErrors := HSErrors or AP_FP_Greater(L21 / L11, 1.2);

          //
          // Build Akima spline
          // Test for general interpolation scheme properties:
          // * values at nodes
          // * continuous function
          // * continuous first derivative
          // Test for specific properties is implemented below.
          //
          if n >= 5 then
            begin
              Spline1DBuildAkima(x, y, n, c);
              Err := 0;
              i := 0;
              while i <= n - 1 do
                begin
                  Err := Max(Err, AbsReal(y[i] - Spline1DCalc(c, x[i])));
                  inc(i);
                end;
              ASErrors := ASErrors or AP_FP_Greater(Err, Threshold);
              LConst(a, b, c, LStep, L10, L11, L12);
              LConst(a, b, c, LStep / 3, L20, L21, L22);
              HSErrors := HSErrors or AP_FP_Greater(L20 / L10, 1.2);
              HSErrors := HSErrors or AP_FP_Greater(L21 / L11, 1.2);
            end;
          inc(pass);
        end;
      inc(n);
    end;

  //
  // Special linear spline test:
  // test for linearity between x[i] and x[i+1]
  //
  n := 2;
  while n <= MaxN do
    begin
      SetLength(x, n (* - 1 + 1 // optimized compiler *) );
      SetLength(y, n (* - 1 + 1 // optimized compiler *) );

      //
      // Prepare task
      //
      a := -1;
      b := +1;
      i := 0;
      while i <= n - 1 do
        begin
          x[i] := a + (b - a) * i / (n - 1);
          y[i] := 2 * RandomReal - 1;
          inc(i);
        end;
      Spline1DBuildLinear(x, y, n, c);

      //
      // Test
      //
      Err := 0;
      k := 0;
      while k <= n - 2 do
        begin
          a := x[k];
          b := x[k + 1];
          pass := 1;
          while pass <= PassCount do
            begin
              t := a + (b - a) * RandomReal;
              v := y[k] + (t - a) / (b - a) * (y[k + 1] - y[k]);
              Err := Max(Err, AbsReal(Spline1DCalc(c, t) - v));
              inc(pass);
            end;
          inc(k);
        end;
      LSErrors := LSErrors or AP_FP_Greater(Err, Threshold);
      inc(n);
    end;

  //
  // Special Akima test: test outlier sensitivity
  // Spline value at (x[i], x[i+1]) should depend from
  // f[i-2], f[i-1], f[i], f[i+1], f[i+2], f[i+3] only.
  //
  n := 5;
  while n <= MaxN do
    begin
      SetLength(x, n (* - 1 + 1 // optimized compiler *) );
      SetLength(y, n (* - 1 + 1 // optimized compiler *) );
      SetLength(y2, n (* - 1 + 1 // optimized compiler *) );

      //
      // Prepare unperturbed Akima spline
      //
      a := -1;
      b := +1;
      i := 0;
      while i <= n - 1 do
        begin
          x[i] := a + (b - a) * i / (n - 1);
          y[i] := Cos(1.3 * pi * x[i] + 0.4);
          inc(i);
        end;
      Spline1DBuildAkima(x, y, n, c);

      //
      // Process perturbed tasks
      //
      Err := 0;
      k := 0;
      while k <= n - 1 do
        begin
          APVMove(@y2[0], 0, n - 1, @y[0], 0, n - 1);
          y2[k] := 5;
          Spline1DBuildAkima(x, y2, n, c2);

          //
          // Test left part independence
          //
          if k - 3 >= 1 then
            begin
              a := -1;
              b := x[k - 3];
              pass := 1;
              while pass <= PassCount do
                begin
                  t := a + (b - a) * RandomReal;
                  Err := Max(Err, AbsReal(Spline1DCalc(c, t) - Spline1DCalc(c2, t)));
                  inc(pass);
                end;
            end;

          //
          // Test right part independence
          //
          if k + 3 <= n - 2 then
            begin
              a := x[k + 3];
              b := +1;
              pass := 1;
              while pass <= PassCount do
                begin
                  t := a + (b - a) * RandomReal;
                  Err := Max(Err, AbsReal(Spline1DCalc(c, t) - Spline1DCalc(c2, t)));
                  inc(pass);
                end;
            end;
          inc(k);
        end;
      ASErrors := ASErrors or AP_FP_Greater(Err, Threshold);
      inc(n);
    end;

  //
  // Differentiation, copy/unpack test
  //
  n := 2;
  while n <= MaxN do
    begin
      SetLength(x, n (* - 1 + 1 // optimized compiler *) );
      SetLength(y, n (* - 1 + 1 // optimized compiler *) );

      //
      // Prepare cubic spline
      //
      a := -1 - RandomReal;
      b := +1 + RandomReal;
      i := 0;
      while i <= n - 1 do
        begin
          x[i] := a + (b - a) * i / (n - 1);
          y[i] := Cos(1.3 * pi * x[i] + 0.4);
          inc(i);
        end;
      Spline1DBuildCubic(x, y, n, 2, 0.0, 2, 0.0, c);

      //
      // Test diff
      //
      Err := 0;
      pass := 1;
      while pass <= PassCount do
        begin
          t := a + (b - a) * RandomReal;
          Spline1DDiff(c, t, s, DS, D2S);
          vl := Spline1DCalc(c, t - h);
          VM := Spline1DCalc(c, t);
          vr := Spline1DCalc(c, t + h);
          Err := Max(Err, AbsReal(s - VM));
          Err := Max(Err, AbsReal(DS - (vr - vl) / (2 * h)));
          Err := Max(Err, AbsReal(D2S - (vr - 2 * VM + vl) / AP_Sqr(h)));
          inc(pass);
        end;
      DSErrors := DSErrors or AP_FP_Greater(Err, 0.001);

      //
      // Test copy
      //
      UnsetSpline1D(c2);
      Spline1DCopy(c, c2);
      Err := 0;
      pass := 1;
      while pass <= PassCount do
        begin
          t := a + (b - a) * RandomReal;
          Err := Max(Err, AbsReal(Spline1DCalc(c, t) - Spline1DCalc(c2, t)));
          inc(pass);
        end;
      CPErrors := CPErrors or AP_FP_Greater(Err, Threshold);

      //
      // Test unpack
      //
      UPErrors := UPErrors or not TestUnpack(c, x);

      //
      // Test lin.trans.
      //
      Err := 0;
      pass := 1;
      while pass <= PassCount do
        begin

          //
          // LinTransX, general A
          //
          SA := 4 * RandomReal - 2;
          SB := 2 * RandomReal - 1;
          t := a + (b - a) * RandomReal;
          Spline1DCopy(c, c2);
          Spline1DLinTransX(c2, SA, SB);
          Err := Max(Err, AbsReal(Spline1DCalc(c, t) - Spline1DCalc(c2,
            (t - SB) / SA)));

          //
          // LinTransX, special case: A=0
          //
          SB := 2 * RandomReal - 1;
          t := a + (b - a) * RandomReal;
          Spline1DCopy(c, c2);
          Spline1DLinTransX(c2, 0, SB);
          Err := Max(Err, AbsReal(Spline1DCalc(c, SB) - Spline1DCalc(c2, t)));

          //
          // LinTransY
          //
          SA := 2 * RandomReal - 1;
          SB := 2 * RandomReal - 1;
          t := a + (b - a) * RandomReal;
          Spline1DCopy(c, c2);
          Spline1DLinTransY(c2, SA, SB);
          Err := Max(Err, AbsReal(SA * Spline1DCalc(c, t) + SB -
            Spline1DCalc(c2, t)));
          inc(pass);
        end;
      LTErrors := LTErrors or AP_FP_Greater(Err, Threshold);
      inc(n);
    end;

  //
  // Testing integration.
  // Three tests are performed:
  //
  // * approximate test (well behaved smooth function, many points,
  // integration inside [a,b]), non-periodic spline
  //
  // * exact test (integration of parabola, outside of [a,b], non-periodic spline
  //
  // * approximate test for periodic splines. F(x)=cos(2*pi*x)+1.
  // Period length is equals to 1.0, so all operations with
  // multiples of period are done exactly. For each value of PERIOD
  // we calculate and test integral at four points:
  // -   0 < t0 < PERIOD
  // -   t1 = PERIOD-eps
  // -   t2 = PERIOD
  // -   t3 = PERIOD+eps
  //
  Err := 0;
  n := 20;
  while n <= 35 do
    begin
      SetLength(x, n (* - 1 + 1 // optimized compiler *) );
      SetLength(y, n (* - 1 + 1 // optimized compiler *) );
      pass := 1;
      while pass <= PassCount do
        begin

          //
          // Prepare cubic spline
          //
          a := -1 - 0.2 * RandomReal;
          b := +1 + 0.2 * RandomReal;
          i := 0;
          while i <= n - 1 do
            begin
              x[i] := a + (b - a) * i / (n - 1);
              y[i] := Sin(pi * x[i] + 0.4) + Exp(x[i]);
              inc(i);
            end;
          BL := pi * Cos(pi * a + 0.4) + Exp(a);
          BR := pi * Cos(pi * b + 0.4) + Exp(b);
          Spline1DBuildCubic(x, y, n, 1, BL, 1, BR, c);

          //
          // Test
          //
          t := a + (b - a) * RandomReal;
          v := -Cos(pi * a + 0.4) / pi + Exp(a);
          v := -Cos(pi * t + 0.4) / pi + Exp(t) - v;
          v := v - Spline1DIntegrate(c, t);
          Err := Max(Err, AbsReal(v));
          inc(pass);
        end;
      inc(n);
    end;
  IErrors := IErrors or AP_FP_Greater(Err, 0.001);
  P0 := 2 * RandomReal - 1;
  p1 := 2 * RandomReal - 1;
  p2 := 2 * RandomReal - 1;
  a := -RandomReal - 0.5;
  b := +RandomReal + 0.5;
  n := 2;
  SetLength(x, n);
  SetLength(y, n);
  SetLength(d, n);
  x[0] := a;
  y[0] := P0 + p1 * a + p2 * AP_Sqr(a);
  d[0] := p1 + 2 * p2 * a;
  x[1] := b;
  y[1] := P0 + p1 * b + p2 * AP_Sqr(b);
  d[1] := p1 + 2 * p2 * b;
  Spline1DBuildHermite(x, y, d, n, c);
  BL := Min(a, b) - AbsReal(b - a);
  BR := Min(a, b) + AbsReal(b - a);
  Err := 0;
  pass := 1;
  while pass <= 100 do
    begin
      t := BL + (BR - BL) * RandomReal;
      v := P0 * t + p1 * AP_Sqr(t) / 2 + p2 * AP_Sqr(t) * t / 3 -
        (P0 * a + p1 * AP_Sqr(a) / 2 + p2 * AP_Sqr(a) * a / 3);
      v := v - Spline1DIntegrate(c, t);
      Err := Max(Err, AbsReal(v));
      inc(pass);
    end;
  IErrors := IErrors or AP_FP_Greater(Err, Threshold);
  n := 100;
  SetLength(x, n);
  SetLength(y, n);
  i := 0;
  while i <= n - 1 do
    begin
      x[i] := AP_Float(i) / (n - 1);
      y[i] := Cos(2 * pi * x[i]) + 1;
      inc(i);
    end;
  y[0] := 2;
  y[n - 1] := 2;
  Spline1DBuildCubic(x, y, n, -1, 0.0, -1, 0.0, c);
  IntAB := Spline1DIntegrate(c, 1.0);
  v := RandomReal;
  vr := Spline1DIntegrate(c, v);
  IErrors := IErrors or AP_FP_Greater(AbsReal(IntAB - 1), 0.001);
  i := -10;
  while i <= 10 do
    begin
      IErrors := IErrors or
        AP_FP_Greater(AbsReal(Spline1DIntegrate(c, i + v) -
        (i * IntAB + vr)), 0.001);
      IErrors := IErrors or
        AP_FP_Greater(AbsReal(Spline1DIntegrate(c, i - 1000 * MachineEpsilon) - i
        * IntAB), 0.001);
      IErrors := IErrors or
        AP_FP_Greater(AbsReal(Spline1DIntegrate(c, i) - i * IntAB), 0.001);
      IErrors := IErrors or
        AP_FP_Greater(AbsReal(Spline1DIntegrate(c, i + 1000 * MachineEpsilon) - i
        * IntAB), 0.001);
      inc(i);
    end;

  //
  // Test fitting.
  //
  pass := 1;
  while pass <= PassCount do
    begin

      //
      // Cubic splines
      // Ability to handle boundary constraints (1-4 constraints on F, dF/dx).
      //
      M := 4;
      while M <= 8 do
        begin
          k := 1;
          while k <= 4 do
            begin
              if k >= M then
                begin
                  inc(k);
                  Continue;
                end;
              n := 100;
              SetLength(x, n);
              SetLength(y, n);
              SetLength(w, n);
              SetLength(XC, 4);
              SetLength(yc, 4);
              SetLength(DC, 4);
              SA := 1 + RandomReal;
              SB := 2 * RandomReal - 1;
              i := 0;
              while i <= n - 1 do
                begin
                  x[i] := SA * RandomReal + SB;
                  y[i] := 2 * RandomReal - 1;
                  w[i] := 1 + RandomReal;
                  inc(i);
                end;
              XC[0] := SB;
              yc[0] := 2 * RandomReal - 1;
              DC[0] := 0;
              XC[1] := SB;
              yc[1] := 2 * RandomReal - 1;
              DC[1] := 1;
              XC[2] := SA + SB;
              yc[2] := 2 * RandomReal - 1;
              DC[2] := 0;
              XC[3] := SA + SB;
              yc[3] := 2 * RandomReal - 1;
              DC[3] := 1;
              Spline1DFitCubicWC(x, y, w, n, XC, yc, DC, k, M, Info, c, Rep);
              if Info <= 0 then
                begin
                  FitErrors := True;
                end
              else
                begin

                  //
                  // Check that constraints are satisfied
                  //
                  i := 0;
                  while i <= k - 1 do
                    begin
                      Spline1DDiff(c, XC[i], s, DS, D2S);
                      if DC[i] = 0 then
                        begin
                          FitErrors := FitErrors or AP_FP_Greater(AbsReal(s - yc[i]),
                            Threshold);
                        end;
                      if DC[i] = 1 then
                        begin
                          FitErrors := FitErrors or AP_FP_Greater(AbsReal(DS - yc[i]),
                            Threshold);
                        end;
                      if DC[i] = 2 then
                        begin
                          FitErrors := FitErrors or AP_FP_Greater(AbsReal(D2S - yc[i]),
                            Threshold);
                        end;
                      inc(i);
                    end;
                end;
              inc(k);
            end;
          inc(M);
        end;

      //
      // Cubic splines
      // Ability to handle one internal constraint
      //
      M := 4;
      while M <= 8 do
        begin
          n := 100;
          SetLength(x, n);
          SetLength(y, n);
          SetLength(w, n);
          SetLength(XC, 1);
          SetLength(yc, 1);
          SetLength(DC, 1);
          SA := 1 + RandomReal;
          SB := 2 * RandomReal - 1;
          i := 0;
          while i <= n - 1 do
            begin
              x[i] := SA * RandomReal + SB;
              y[i] := 2 * RandomReal - 1;
              w[i] := 1 + RandomReal;
              inc(i);
            end;
          XC[0] := SA * RandomReal + SB;
          yc[0] := 2 * RandomReal - 1;
          DC[0] := RandomInteger(2);
          Spline1DFitCubicWC(x, y, w, n, XC, yc, DC, 1, M, Info, c, Rep);
          if Info <= 0 then
            begin
              FitErrors := True;
            end
          else
            begin

              //
              // Check that constraints are satisfied
              //
              Spline1DDiff(c, XC[0], s, DS, D2S);
              if DC[0] = 0 then
                begin
                  FitErrors := FitErrors or AP_FP_Greater(AbsReal(s - yc[0]),
                    Threshold);
                end;
              if DC[0] = 1 then
                begin
                  FitErrors := FitErrors or AP_FP_Greater(AbsReal(DS - yc[0]),
                    Threshold);
                end;
              if DC[0] = 2 then
                begin
                  FitErrors := FitErrors or AP_FP_Greater(AbsReal(D2S - yc[0]),
                    Threshold);
                end;
            end;
          inc(M);
        end;

      //
      // Hermite splines
      // Ability to handle boundary constraints (1-4 constraints on F, dF/dx).
      //
      M := 4;
      while M <= 8 do
        begin
          k := 1;
          while k <= 4 do
            begin
              if k >= M then
                begin
                  inc(k);
                  Continue;
                end;
              if M mod 2 <> 0 then
                begin
                  inc(k);
                  Continue;
                end;
              n := 100;
              SetLength(x, n);
              SetLength(y, n);
              SetLength(w, n);
              SetLength(XC, 4);
              SetLength(yc, 4);
              SetLength(DC, 4);
              SA := 1 + RandomReal;
              SB := 2 * RandomReal - 1;
              i := 0;
              while i <= n - 1 do
                begin
                  x[i] := SA * RandomReal + SB;
                  y[i] := 2 * RandomReal - 1;
                  w[i] := 1 + RandomReal;
                  inc(i);
                end;
              XC[0] := SB;
              yc[0] := 2 * RandomReal - 1;
              DC[0] := 0;
              XC[1] := SB;
              yc[1] := 2 * RandomReal - 1;
              DC[1] := 1;
              XC[2] := SA + SB;
              yc[2] := 2 * RandomReal - 1;
              DC[2] := 0;
              XC[3] := SA + SB;
              yc[3] := 2 * RandomReal - 1;
              DC[3] := 1;
              Spline1DFitHermiteWC(x, y, w, n, XC, yc, DC, k, M, Info, c, Rep);
              if Info <= 0 then
                begin
                  FitErrors := True;
                end
              else
                begin

                  //
                  // Check that constraints are satisfied
                  //
                  i := 0;
                  while i <= k - 1 do
                    begin
                      Spline1DDiff(c, XC[i], s, DS, D2S);
                      if DC[i] = 0 then
                        begin
                          FitErrors := FitErrors or AP_FP_Greater(AbsReal(s - yc[i]),
                            Threshold);
                        end;
                      if DC[i] = 1 then
                        begin
                          FitErrors := FitErrors or AP_FP_Greater(AbsReal(DS - yc[i]),
                            Threshold);
                        end;
                      if DC[i] = 2 then
                        begin
                          FitErrors := FitErrors or AP_FP_Greater(AbsReal(D2S - yc[i]),
                            Threshold);
                        end;
                      inc(i);
                    end;
                end;
              inc(k);
            end;
          inc(M);
        end;

      //
      // Hermite splines
      // Ability to handle one internal constraint
      //
      M := 4;
      while M <= 8 do
        begin
          if M mod 2 <> 0 then
            begin
              inc(M);
              Continue;
            end;
          n := 100;
          SetLength(x, n);
          SetLength(y, n);
          SetLength(w, n);
          SetLength(XC, 1);
          SetLength(yc, 1);
          SetLength(DC, 1);
          SA := 1 + RandomReal;
          SB := 2 * RandomReal - 1;
          i := 0;
          while i <= n - 1 do
            begin
              x[i] := SA * RandomReal + SB;
              y[i] := 2 * RandomReal - 1;
              w[i] := 1 + RandomReal;
              inc(i);
            end;
          XC[0] := SA * RandomReal + SB;
          yc[0] := 2 * RandomReal - 1;
          DC[0] := RandomInteger(2);
          Spline1DFitHermiteWC(x, y, w, n, XC, yc, DC, 1, M, Info, c, Rep);
          if Info <= 0 then
            begin
              FitErrors := True;
            end
          else
            begin

              //
              // Check that constraints are satisfied
              //
              Spline1DDiff(c, XC[0], s, DS, D2S);
              if DC[0] = 0 then
                begin
                  FitErrors := FitErrors or AP_FP_Greater(AbsReal(s - yc[0]),
                    Threshold);
                end;
              if DC[0] = 1 then
                begin
                  FitErrors := FitErrors or AP_FP_Greater(AbsReal(DS - yc[0]),
                    Threshold);
                end;
              if DC[0] = 2 then
                begin
                  FitErrors := FitErrors or AP_FP_Greater(AbsReal(D2S - yc[0]),
                    Threshold);
                end;
            end;
          inc(M);
        end;
      inc(pass);
    end;
  M := 4;
  while M <= 8 do
    begin
      SType := 0;
      while SType <= 1 do
        begin
          pass := 1;
          while pass <= PassCount do
            begin
              if (SType = 1) and (M mod 2 <> 0) then
                begin
                  inc(pass);
                  Continue;
                end;

              //
              // cubic/Hermite spline fitting:
              // * generate "template spline" C2
              // * generate 2*N points from C2, such that result of
              // ideal fit should be equal to C2
              // * fit, store in C
              // * compare C and C2
              //
              SA := 1 + RandomReal;
              SB := 2 * RandomReal - 1;
              if SType = 0 then
                begin
                  SetLength(x, M - 2);
                  SetLength(y, M - 2);
                  i := 0;
                  while i <= M - 2 - 1 do
                    begin
                      x[i] := SA * i / (M - 2 - 1) + SB;
                      y[i] := 2 * RandomReal - 1;
                      inc(i);
                    end;
                  Spline1DBuildCubic(x, y, M - 2, 1, 2 * RandomReal - 1, 1,
                    2 * RandomReal - 1, c2);
                end;
              if SType = 1 then
                begin
                  SetLength(x, M div 2);
                  SetLength(y, M div 2);
                  SetLength(d, M div 2);
                  i := 0;
                  while i <= M div 2 - 1 do
                    begin
                      x[i] := SA * i / (M div 2 - 1) + SB;
                      y[i] := 2 * RandomReal - 1;
                      d[i] := 2 * RandomReal - 1;
                      inc(i);
                    end;
                  Spline1DBuildHermite(x, y, d, M div 2, c2);
                end;
              n := 50;
              SetLength(x, 2 * n);
              SetLength(y, 2 * n);
              SetLength(w, 2 * n);
              i := 0;
              while i <= n - 1 do
                begin

                  //
                  // "if i=0" and "if i=1" are needed to
                  // synchronize interval size for C2 and
                  // spline being fitted (i.e. C).
                  //
                  t := RandomReal;
                  x[i] := SA * RandomReal + SB;
                  if i = 0 then
                    begin
                      x[i] := SB;
                    end;
                  if i = 1 then
                    begin
                      x[i] := SA + SB;
                    end;
                  v := Spline1DCalc(c2, x[i]);
                  y[i] := v + t;
                  w[i] := 1 + RandomReal;
                  x[n + i] := x[i];
                  y[n + i] := v - t;
                  w[n + i] := w[i];
                  inc(i);
                end;
              if SType = 0 then
                begin
                  Spline1DFitCubicWC(x, y, w, 2 * n, XC, yc, DC, 0, M, Info, c, Rep);
                end;
              if SType = 1 then
                begin
                  Spline1DFitHermiteWC(x, y, w, 2 * n, XC, yc, DC, 0, M, Info, c, Rep);
                end;
              if Info <= 0 then
                begin
                  FitErrors := True;
                end
              else
                begin
                  i := 0;
                  while i <= n - 1 do
                    begin
                      v := SA * RandomReal + SB;
                      FitErrors := FitErrors or
                        AP_FP_Greater(AbsReal(Spline1DCalc(c, v) - Spline1DCalc(c2, v)),
                        Threshold);
                      inc(i);
                    end;
                end;
              inc(pass);
            end;
          inc(SType);
        end;
      inc(M);
    end;
  M := 4;
  while M <= 8 do
    begin
      pass := 1;
      while pass <= PassCount do
        begin

          //
          // prepare points/weights
          //
          SA := 1 + RandomReal;
          SB := 2 * RandomReal - 1;
          n := 10 + RandomInteger(10);
          SetLength(x, n);
          SetLength(y, n);
          SetLength(w, n);
          i := 0;
          while i <= n - 1 do
            begin
              x[i] := SA * RandomReal + SB;
              y[i] := 2 * RandomReal - 1;
              w[i] := 1;
              inc(i);
            end;

          //
          // Fit cubic with unity weights, without weights, then compare
          //
          if M >= 4 then
            begin
              Spline1DFitCubicWC(x, y, w, n, XC, yc, DC, 0, M, Info1, c, Rep);
              Spline1DFitCubic(x, y, n, M, Info2, c2, Rep2);
              if (Info1 <= 0) or (Info2 <= 0) then
                begin
                  FitErrors := True;
                end
              else
                begin
                  i := 0;
                  while i <= n - 1 do
                    begin
                      v := SA * RandomReal + SB;
                      FitErrors := FitErrors or AP_FP_NEq(Spline1DCalc(c, v),
                        Spline1DCalc(c2, v));
                      FitErrors := FitErrors or AP_FP_NEq(Rep.TaskRCond, Rep2.TaskRCond);
                      FitErrors := FitErrors or AP_FP_NEq(Rep.RMSError, Rep2.RMSError);
                      FitErrors := FitErrors or AP_FP_NEq(Rep.AvgError, Rep2.AvgError);
                      FitErrors := FitErrors or AP_FP_NEq(Rep.AvgRelError,
                        Rep2.AvgRelError);
                      FitErrors := FitErrors or AP_FP_NEq(Rep.MaxError, Rep2.MaxError);
                      inc(i);
                    end;
                end;
            end;

          //
          // Fit Hermite with unity weights, without weights, then compare
          //
          if (M >= 4) and (M mod 2 = 0) then
            begin
              Spline1DFitHermiteWC(x, y, w, n, XC, yc, DC, 0, M, Info1, c, Rep);
              Spline1DFitHermite(x, y, n, M, Info2, c2, Rep2);
              if (Info1 <= 0) or (Info2 <= 0) then
                begin
                  FitErrors := True;
                end
              else
                begin
                  i := 0;
                  while i <= n - 1 do
                    begin
                      v := SA * RandomReal + SB;
                      FitErrors := FitErrors or AP_FP_NEq(Spline1DCalc(c, v),
                        Spline1DCalc(c2, v));
                      FitErrors := FitErrors or AP_FP_NEq(Rep.TaskRCond, Rep2.TaskRCond);
                      FitErrors := FitErrors or AP_FP_NEq(Rep.RMSError, Rep2.RMSError);
                      FitErrors := FitErrors or AP_FP_NEq(Rep.AvgError, Rep2.AvgError);
                      FitErrors := FitErrors or AP_FP_NEq(Rep.AvgRelError,
                        Rep2.AvgRelError);
                      FitErrors := FitErrors or AP_FP_NEq(Rep.MaxError, Rep2.MaxError);
                      inc(i);
                    end;
                end;
            end;
          inc(pass);
        end;
      inc(M);
    end;
  pass := 1;
  while pass <= PassCount do
    begin
      Assert(PassCount >= 2, 'PassCount should be 2 or greater!');

      //
      // solve simple task (all X[] are the same, Y[] are specially
      // calculated to ensure simple form of all types of errors)
      // and check correctness of the errors calculated by subroutines
      //
      // First pass is done with zero Y[], other passes - with random Y[].
      // It should test both ability to correctly calculate errors and
      // ability to not fail while working with zeros :)
      //
      n := 4;
      if pass = 1 then
        begin
          v1 := 0;
          v2 := 0;
          v := 0;
        end
      else
        begin
          v1 := RandomReal;
          v2 := RandomReal;
          v := 1 + RandomReal;
        end;
      SetLength(x, 4);
      SetLength(y, 4);
      SetLength(w, 4);
      x[0] := 0;
      y[0] := v - v2;
      w[0] := 1;
      x[1] := 0;
      y[1] := v - v1;
      w[1] := 1;
      x[2] := 0;
      y[2] := v + v1;
      w[2] := 1;
      x[3] := 0;
      y[3] := v + v2;
      w[3] := 1;
      RefRMS := Sqrt((AP_Sqr(v1) + AP_Sqr(v2)) / 2);
      RefAvg := (AbsReal(v1) + AbsReal(v2)) / 2;
      if pass = 1 then
        begin
          RefAvgRel := 0;
        end
      else
        begin
          RefAvgRel := 0.25 * (AbsReal(v2) / AbsReal(v - v2) + AbsReal(v1) /
            AbsReal(v - v1) + AbsReal(v1) / AbsReal(v + v1) + AbsReal(v2) /
            AbsReal(v + v2));
        end;
      RefMax := Max(v1, v2);

      //
      // Test cubic fitting
      //
      Spline1DFitCubic(x, y, 4, 4, Info, c, Rep);
      if Info <= 0 then
        begin
          FitErrors := True;
        end
      else
        begin
          s := Spline1DCalc(c, 0);
          FitErrors := FitErrors or AP_FP_Greater(AbsReal(s - v), Threshold);
          FitErrors := FitErrors or AP_FP_Greater(AbsReal(Rep.RMSError - RefRMS),
            Threshold);
          FitErrors := FitErrors or AP_FP_Greater(AbsReal(Rep.AvgError - RefAvg),
            Threshold);
          FitErrors := FitErrors or
            AP_FP_Greater(AbsReal(Rep.AvgRelError - RefAvgRel), Threshold);
          FitErrors := FitErrors or AP_FP_Greater(AbsReal(Rep.MaxError - RefMax),
            Threshold);
        end;

      //
      // Test cubic fitting
      //
      Spline1DFitHermite(x, y, 4, 4, Info, c, Rep);
      if Info <= 0 then
        begin
          FitErrors := True;
        end
      else
        begin
          s := Spline1DCalc(c, 0);
          FitErrors := FitErrors or AP_FP_Greater(AbsReal(s - v), Threshold);
          FitErrors := FitErrors or AP_FP_Greater(AbsReal(Rep.RMSError - RefRMS),
            Threshold);
          FitErrors := FitErrors or AP_FP_Greater(AbsReal(Rep.AvgError - RefAvg),
            Threshold);
          FitErrors := FitErrors or
            AP_FP_Greater(AbsReal(Rep.AvgRelError - RefAvgRel), Threshold);
          FitErrors := FitErrors or AP_FP_Greater(AbsReal(Rep.MaxError - RefMax),
            Threshold);
        end;
      inc(pass);
    end;

  //
  // report
  //
  WasErrors := LSErrors or CSErrors or CRSErrors or HSErrors or ASErrors or
    DSErrors or CPErrors or UPErrors or LTErrors or IErrors or FitErrors;
  if not Silent then
    begin
      DoStatusNoLn(PFormat('TESTING SPLINE INTERPOLATION'#13#10'', []));

      //
      // Normal tests
      //
      DoStatusNoLn(PFormat('LINEAR SPLINE TEST:                      ', []));
      if LSErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('CUBIC SPLINE TEST:                       ', []));
      if CSErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('CATMULL-ROM SPLINE TEST:                 ', []));
      if CRSErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('HERMITE SPLINE TEST:                     ', []));
      if HSErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('AKIMA SPLINE TEST:                       ', []));
      if ASErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('DIFFERENTIATION TEST:                    ', []));
      if DSErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('COPY/SERIALIZATION TEST:                 ', []));
      if CPErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('UNPACK TEST:                             ', []));
      if UPErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('LIN.TRANS. TEST:                         ', []));
      if LTErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('INTEGRATION TEST:                        ', []));
      if IErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('FITTING TEST:                            ', []));
      if FitErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      if WasErrors then
        begin
          DoStatusNoLn(PFormat('TEST FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('TEST PASSED'#13#10'', []));
        end;
      DoStatusNoLn(PFormat(''#13#10''#13#10'', []));
    end;

  //
  // end
  //
  Result := not WasErrors;
end;

procedure ExpressionToMatrixTest();
const
  c1 =
    '1*1,1*2,1*3,' +
    '2*1,2*2,2*3,' +
    '3*1,3*2,3*3,' +
    '4*4,4*5,4*6';

  c2 =
    '1,2,3,' +
    '4,5,6,' +
    '7,8,9,' +
    '10,11,12';

  CV1 = 'IfThen(1=1,0.1,10000),clamp(2,0.1,0.2),max(0.001,0.01,0.1)';
  CV2 = 'sqrt(a1),clamp(a2,0.1,0.2),max(a1,a2,a3)';
var
  M1, M2: TLMatrix;
  v1, v2: TLVec;
  const_vl: THashVariantList;
begin
  M1 := ExpressionToLMatrix(3, 4, c1);
  M2 := ExpressionToLMatrix(3, 4, c2);
  v1 := ExpressionToLVec(CV1);

  const_vl := THashVariantList.Create;
  const_vl['a1'] := 3;
  const_vl['a2'] := 3 * 0.5; // half 3
  const_vl['a3'] := 3 * 2;
  v2 := ExpressionToLVec(CV2, const_vl);
  disposeObject(const_vl);
end;

procedure LearnTest();
const
  test_InLen = 5;
  test_OutLen = 5;
  test_Depth = 10;

  RegressionLearnType: set of TLearnType = [
    ltKDT, ltKM,
    ltForest,
    ltLM, ltLM_MT, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod, ltMonteCarlo,
    ltLM_Ensemble, ltLM_Ensemble_MT, ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT];

  ClassifierLearnType: set of TLearnType = [
    ltKDT, ltKM,
    ltForest, ltLogit,
    ltLM, ltLM_MT, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod, ltMonteCarlo,
    ltLM_Ensemble, ltLM_Ensemble_MT, ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT];

var
  lt: TLearnType;
  lr: TLearn;
  i, j, LV: TLInt;
  f_In, f_Out: TLVec;
  f_out_list: TLMatrix;
  Search_Out: TLIVec;
  M: TLMatrix;
  m64: TMemoryStream64;
  md5_1, md5_2: TMD5;
begin
  DoStatus('Begin ABLAS');
  TestABLAS(False);
  DoStatus('Begin XBLAS');
  TestXBLAS(False);
  DoStatus('Begin Gauss Kronrod Quadrature');
  TestGaussKronrodQuadrature(False);
  DoStatus('Begin Gauss Quadrature');
  TestGaussQuadrature(False);
  DoStatus('Begin MinLBFGS');
  TestMinLBFGS(False);
  DoStatus('Begin MinLM');
  TestMinLM(False);
  DoStatus('Begin linear fitting');
  TestLLS(False);
  DoStatus('Begin Polynomial fitting');
  TestPolInt(False);
  DoStatus('Begin Spline1D fitting');
  TestSplineInterpolation(False);
  DoStatus('Begin ExpressionToMatrix');
  ExpressionToMatrixTest;
  DoStatus('Begin Learn Engine');
  // Regression test
  for LV := 0 to 2 do
    for lt in RegressionLearnType do
      begin
        DoStatusNoLn('Level-%d test %s', [LV, CLearnString[lt]]);
        case LV of
          0: lr := TLearn.CreateRegression(lt, test_InLen, test_OutLen);
          1: lr := TLearn.CreateRegression1(lt, test_InLen, test_OutLen);
          else lr := TLearn.CreateRegression2(lt, test_InLen, test_OutLen);
        end;

        SetLength(f_In, lr.FInSize);
        SetLength(f_Out, lr.FOutSize);

        for j := 0 to test_Depth - 1 do
          begin
            for i := 0 to lr.FInSize - 1 do
                f_In[i] := umlRandomRangeD(0, 100);
            for i := 0 to lr.FOutSize - 1 do
                f_Out[i] := umlRandomRangeD(0, 10);
            lr.AddMemory(f_In, f_Out);
          end;

        DoStatusNoLn('...Training');
        if lr.Training then
          begin
            SetLength(f_out_list, lr.Count);
            for i := 0 to lr.Count - 1 do
              begin
                try
                  if not lr.process(@lr[i]^.m_in, @f_out_list[i]) then
                      DoStatusNoLn('...error');
                except
                end;
              end;

            for i := 0 to lr.Count - 1 do
              begin
                lr.SearchMemoryPearson(lr[i]^.m_in);
                lr.SearchMemoryPearson(lr[i]^.m_in, Search_Out);
                SetLength(Search_Out, 0);

                lr.SearchMemorySpearman(lr[i]^.m_in);
                lr.SearchMemorySpearman(lr[i]^.m_in, Search_Out);
                SetLength(Search_Out, 0);

                lr.SearchMemoryDistance(lr[i]^.m_in);
                lr.SearchMemoryDistance(lr[i]^.m_in, Search_Out);
                SetLength(Search_Out, 0);
              end;

            DoStatusNoLn('...serialize');
            m64 := TMemoryStream64.Create;
            lr.SaveToStream(m64);
            md5_1 := umlStreamMD5(m64);
            m64.Position := 0;
            lr.LoadFromStream(m64);
            m64.Clear;
            lr.SaveToStream(m64);
            md5_2 := umlStreamMD5(m64);
            if not umlCompareMD5(md5_1, md5_2) then
                DoStatusNoLn('...error ');
            disposeObject(m64);

            for i := 0 to lr.Count - 1 do
              if lr.process(@lr[i]^.m_in, @f_Out) then
                begin
                  if TKDTree.Distance(f_Out, f_out_list[i]) <> 0 then
                      DoStatusNoLn('...verify data error ');
                end
              else
                  DoStatusNoLn('...verify data with process time error ');

            SetLength(f_out_list, 0, 0);
          end
        else
            DoStatusNoLn('...Training error');

        DoStatusNoLn('...ok!');
        DoStatusNoLn;

        SetLength(f_In, 0);
        SetLength(f_Out, 0);

        disposeObject(lr);
      end;

  // classifier test
  for LV := 0 to 2 do
    for lt in ClassifierLearnType do
      begin
        DoStatusNoLn('Classifier%d test %s ..', [LV, CLearnString[lt]]);
        case LV of
          0: lr := TLearn.CreateClassifier(lt, test_InLen);
          1: lr := TLearn.CreateClassifier1(lt, test_InLen);
          else lr := TLearn.CreateClassifier2(lt, test_InLen);
        end;

        SetLength(f_In, lr.FInSize);
        SetLength(f_Out, lr.FOutSize);

        for j := 0 to test_Depth - 1 do
          begin
            for i := 0 to lr.FInSize - 1 do
                f_In[i] := umlRandomRangeD(0, 100);
            for i := 0 to lr.FOutSize - 1 do
                f_Out[i] := umlRandomRangeD(0, 10);
            lr.AddMemory(f_In, f_Out);
          end;
        if lr.Training() then
          begin
            SetLength(f_out_list, lr.Count);
            for i := 0 to lr.Count - 1 do
              if not lr.process(@lr[i]^.m_in, @f_out_list[i]) then
                  DoStatusNoLn('process error ');

            m64 := TMemoryStream64.Create;
            lr.SaveToStream(m64);
            md5_1 := umlStreamMD5(m64);
            m64.Position := 0;
            lr.LoadFromStream(m64);
            m64.Clear;
            lr.SaveToStream(m64);
            md5_2 := umlStreamMD5(m64);
            if not umlCompareMD5(md5_1, md5_2) then
                DoStatusNoLn('encode error ');
            disposeObject(m64);

            for i := 0 to lr.Count - 1 do
              if lr.process(@lr[i]^.m_in, @f_Out) then
                begin
                  if TKDTree.Distance(f_Out, f_out_list[i]) <> 0 then
                      DoStatusNoLn('verify data error ');
                end
              else
                  DoStatusNoLn('process verify error ');

            SetLength(f_out_list, 0, 0);
          end
        else
            DoStatusNoLn('Training error ');

        DoStatusNoLn;

        SetLength(f_In, 0);
        SetLength(f_Out, 0);

        disposeObject(lr);
      end;

  DoStatus('all learn test finish!');
end;
