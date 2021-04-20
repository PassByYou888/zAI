{ ****************************************************************************** }
{ * Generation of random matrix support, by QQ 600585@qq.com                   * }
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
  Generation of a random uniformly distributed (Haar) orthogonal matrix

  INPUT PARAMETERS:
  N   -   matrix size, N>=1

  OUTPUT PARAMETERS:
  A   -   orthogonal NxN matrix, array[0..N-1,0..N-1]
  ************************************************************************ *)
procedure RMatrixRndOrthogonal(n: TLInt; var a: TLMatrix);
var
  i: TLInt;
  j: TLInt;
begin
  Assert(n >= 1, 'RMatrixRndOrthogonal: N<1!');
  SetLength(a, n (* - 1 + 1 // optimized compiler *) , n (* - 1 + 1 // optimized compiler *) );
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          if i = j then
            begin
              a[i, j] := 1;
            end
          else
            begin
              a[i, j] := 0;
            end;
          inc(j);
        end;
      inc(i);
    end;
  RMatrixRndOrthogonalFromTheRight(a, n, n);
end;

(* ************************************************************************
  Generation of random NxN matrix with given condition number and norm2(A)=1

  INPUT PARAMETERS:
  N   -   matrix size
  C   -   condition number (in 2-norm)

  OUTPUT PARAMETERS:
  A   -   random matrix with norm2(A)=1 and cond(A)=C
  ************************************************************************ *)
procedure RMatrixRndCond(n: TLInt; c: TLFloat; var a: TLMatrix);
var
  i: TLInt;
  j: TLInt;
  L1: TLFloat;
  l2: TLFloat;
begin
  Assert((n >= 1) and AP_FP_Greater_Eq(c, 1), 'RMatrixRndCond: N<1 or C<1!');
  SetLength(a, n (* - 1 + 1 // optimized compiler *) , n (* - 1 + 1 // optimized compiler *) );
  if n = 1 then
    begin

      //
      // special case
      //
      a[0, 0] := 2 * RandomInteger(2) - 1;
      Exit;
    end;
  L1 := 0;
  l2 := ln(1 / c);
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          a[i, j] := 0;
          inc(j);
        end;
      inc(i);
    end;
  a[0, 0] := Exp(L1);
  i := 1;
  while i <= n - 2 do
    begin
      a[i, i] := Exp(RandomReal * (l2 - L1) + L1);
      inc(i);
    end;
  a[n - 1, n - 1] := Exp(l2);
  RMatrixRndOrthogonalFromTheLeft(a, n, n);
  RMatrixRndOrthogonalFromTheRight(a, n, n);
end;

(* ************************************************************************
  Generation of a random Haar distributed orthogonal TLComplex matrix

  INPUT PARAMETERS:
  N   -   matrix size, N>=1

  OUTPUT PARAMETERS:
  A   -   orthogonal NxN matrix, array[0..N-1,0..N-1]
  ************************************************************************ *)
procedure CMatrixRndOrthogonal(n: TLInt; var a: TLComplexMatrix);
var
  i: TLInt;
  j: TLInt;
begin
  Assert(n >= 1, 'CMatrixRndOrthogonal: N<1!');
  SetLength(a, n (* - 1 + 1 // optimized compiler *) , n (* - 1 + 1 // optimized compiler *) );
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          if i = j then
            begin
              a[i, j] := C_Complex(1);
            end
          else
            begin
              a[i, j] := C_Complex(0);
            end;
          inc(j);
        end;
      inc(i);
    end;
  CMatrixRndOrthogonalFromTheRight(a, n, n);
end;

(* ************************************************************************
  Generation of random NxN TLComplex matrix with given condition number C and
  norm2(A)=1

  INPUT PARAMETERS:
  N   -   matrix size
  C   -   condition number (in 2-norm)

  OUTPUT PARAMETERS:
  A   -   random matrix with norm2(A)=1 and cond(A)=C
  ************************************************************************ *)
procedure CMatrixRndCond(n: TLInt; c: TLFloat; var a: TLComplexMatrix);
var
  i: TLInt;
  j: TLInt;
  L1: TLFloat;
  l2: TLFloat;
  State: THQRNDState;
  v: TLComplex;
begin
  Assert((n >= 1) and AP_FP_Greater_Eq(c, 1), 'CMatrixRndCond: N<1 or C<1!');
  SetLength(a, n (* - 1 + 1 // optimized compiler *) , n (* - 1 + 1 // optimized compiler *) );
  if n = 1 then
    begin

      //
      // special case
      //
      HQRNDRandomize(State);
      HQRNDUnit2(State, v.x, v.y);
      a[0, 0] := v;
      Exit;
    end;
  L1 := 0;
  l2 := ln(1 / c);
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          a[i, j] := C_Complex(0);
          inc(j);
        end;
      inc(i);
    end;
  a[0, 0] := C_Complex(Exp(L1));
  i := 1;
  while i <= n - 2 do
    begin
      a[i, i] := C_Complex(Exp(RandomReal * (l2 - L1) + L1));
      inc(i);
    end;
  a[n - 1, n - 1] := C_Complex(Exp(l2));
  CMatrixRndOrthogonalFromTheLeft(a, n, n);
  CMatrixRndOrthogonalFromTheRight(a, n, n);
end;

(* ************************************************************************
  Generation of random NxN symmetric matrix with given condition number  and
  norm2(A)=1

  INPUT PARAMETERS:
  N   -   matrix size
  C   -   condition number (in 2-norm)

  OUTPUT PARAMETERS:
  A   -   random matrix with norm2(A)=1 and cond(A)=C
  ************************************************************************ *)
procedure SMatrixRndCond(n: TLInt; c: TLFloat; var a: TLMatrix);
var
  i: TLInt;
  j: TLInt;
  L1: TLFloat;
  l2: TLFloat;
begin
  Assert((n >= 1) and AP_FP_Greater_Eq(c, 1), 'SMatrixRndCond: N<1 or C<1!');
  SetLength(a, n (* - 1 + 1 // optimized compiler *) , n (* - 1 + 1 // optimized compiler *) );
  if n = 1 then
    begin

      //
      // special case
      //
      a[0, 0] := 2 * RandomInteger(2) - 1;
      Exit;
    end;

  //
  // Prepare matrix
  //
  L1 := 0;
  l2 := ln(1 / c);
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          a[i, j] := 0;
          inc(j);
        end;
      inc(i);
    end;
  a[0, 0] := Exp(L1);
  i := 1;
  while i <= n - 2 do
    begin
      a[i, i] := (2 * RandomInteger(2) - 1) * Exp(RandomReal * (l2 - L1) + L1);
      inc(i);
    end;
  a[n - 1, n - 1] := Exp(l2);

  //
  // Multiply
  //
  SMatrixRndMultiply(a, n);
end;

(* ************************************************************************
  Generation of random NxN symmetric positive definite matrix with given
  condition number and norm2(A)=1

  INPUT PARAMETERS:
  N   -   matrix size
  C   -   condition number (in 2-norm)

  OUTPUT PARAMETERS:
  A   -   random SPD matrix with norm2(A)=1 and cond(A)=C
  ************************************************************************ *)
procedure SPDMatrixRndCond(n: TLInt; c: TLFloat;
  var a: TLMatrix);
var
  i: TLInt;
  j: TLInt;
  L1: TLFloat;
  l2: TLFloat;
begin

  //
  // Special cases
  //
  if (n <= 0) or AP_FP_Less(c, 1) then
    begin
      Exit;
    end;
  SetLength(a, n (* - 1 + 1 // optimized compiler *) , n (* - 1 + 1 // optimized compiler *) );
  if n = 1 then
    begin
      a[0, 0] := 1;
      Exit;
    end;

  //
  // Prepare matrix
  //
  L1 := 0;
  l2 := ln(1 / c);
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          a[i, j] := 0;
          inc(j);
        end;
      inc(i);
    end;
  a[0, 0] := Exp(L1);
  i := 1;
  while i <= n - 2 do
    begin
      a[i, i] := Exp(RandomReal * (l2 - L1) + L1);
      inc(i);
    end;
  a[n - 1, n - 1] := Exp(l2);

  //
  // Multiply
  //
  SMatrixRndMultiply(a, n);
end;

(* ************************************************************************
  Generation of random NxN Hermitian matrix with given condition number  and
  norm2(A)=1

  INPUT PARAMETERS:
  N   -   matrix size
  C   -   condition number (in 2-norm)

  OUTPUT PARAMETERS:
  A   -   random matrix with norm2(A)=1 and cond(A)=C
  ************************************************************************ *)
procedure HMatrixRndCond(n: TLInt; c: TLFloat;
  var a: TLComplexMatrix);
var
  i: TLInt;
  j: TLInt;
  L1: TLFloat;
  l2: TLFloat;
begin
  Assert((n >= 1) and AP_FP_Greater_Eq(c, 1), 'HMatrixRndCond: N<1 or C<1!');
  SetLength(a, n (* - 1 + 1 // optimized compiler *) , n (* - 1 + 1 // optimized compiler *) );
  if n = 1 then
    begin

      //
      // special case
      //
      a[0, 0] := C_Complex(2 * RandomInteger(2) - 1);
      Exit;
    end;

  //
  // Prepare matrix
  //
  L1 := 0;
  l2 := ln(1 / c);
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          a[i, j] := C_Complex(0);
          inc(j);
        end;
      inc(i);
    end;
  a[0, 0] := C_Complex(Exp(L1));
  i := 1;
  while i <= n - 2 do
    begin
      a[i, i] := C_Complex((2 * RandomInteger(2) - 1) *
        Exp(RandomReal * (l2 - L1) + L1));
      inc(i);
    end;
  a[n - 1, n - 1] := C_Complex(Exp(l2));

  //
  // Multiply
  //
  HMatrixRndMultiply(a, n);

  //
  // post-process to ensure that matrix diagonal is real
  //
  i := 0;
  while i <= n - 1 do
    begin
      a[i, i].y := 0;
      inc(i);
    end;
end;

(* ************************************************************************
  Generation of random NxN Hermitian positive definite matrix with given
  condition number and norm2(A)=1

  INPUT PARAMETERS:
  N   -   matrix size
  C   -   condition number (in 2-norm)

  OUTPUT PARAMETERS:
  A   -   random HPD matrix with norm2(A)=1 and cond(A)=C
  ************************************************************************ *)
procedure HPDMatrixRndCond(n: TLInt; c: TLFloat;
  var a: TLComplexMatrix);
var
  i: TLInt;
  j: TLInt;
  L1: TLFloat;
  l2: TLFloat;
begin

  //
  // Special cases
  //
  if (n <= 0) or AP_FP_Less(c, 1) then
    begin
      Exit;
    end;
  SetLength(a, n (* - 1 + 1 // optimized compiler *) , n (* - 1 + 1 // optimized compiler *) );
  if n = 1 then
    begin
      a[0, 0] := C_Complex(1);
      Exit;
    end;

  //
  // Prepare matrix
  //
  L1 := 0;
  l2 := ln(1 / c);
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          a[i, j] := C_Complex(0);
          inc(j);
        end;
      inc(i);
    end;
  a[0, 0] := C_Complex(Exp(L1));
  i := 1;
  while i <= n - 2 do
    begin
      a[i, i] := C_Complex(Exp(RandomReal * (l2 - L1) + L1));
      inc(i);
    end;
  a[n - 1, n - 1] := C_Complex(Exp(l2));

  //
  // Multiply
  //
  HMatrixRndMultiply(a, n);

  //
  // post-process to ensure that matrix diagonal is real
  //
  i := 0;
  while i <= n - 1 do
    begin
      a[i, i].y := 0;
      inc(i);
    end;
end;

(* ************************************************************************
  Multiplication of MxN matrix by NxN random Haar distributed orthogonal matrix

  INPUT PARAMETERS:
  A   -   matrix, array[0..M-1, 0..N-1]
  M, N-   matrix size

  OUTPUT PARAMETERS:
  A   -   A*Q, where Q is random NxN orthogonal matrix
  ************************************************************************ *)
procedure RMatrixRndOrthogonalFromTheRight(var a: TLMatrix; M: TLInt; n: TLInt);
var
  TAU: TLFloat;
  LAMBDA: TLFloat;
  s: TLInt;
  i: TLInt;
  U1: TLFloat;
  U2: TLFloat;
  w: TLVec;
  v: TLVec;
  State: THQRNDState;
  i_: TLInt;
begin
  Assert((n >= 1) and (M >= 1),
    'RMatrixRndOrthogonalFromTheRight: N<1 or M<1!');
  if n = 1 then
    begin

      //
      // Special case
      //
      TAU := 2 * RandomInteger(2) - 1;
      i := 0;
      while i <= M - 1 do
        begin
          a[i, 0] := a[i, 0] * TAU;
          inc(i);
        end;
      Exit;
    end;

  //
  // General case.
  // First pass.
  //
  SetLength(w, M (* - 1 + 1 // optimized compiler *) );
  SetLength(v, n + 1);
  HQRNDRandomize(State);
  s := 2;
  while s <= n do
    begin

      //
      // Prepare random normal v
      //
      repeat
        i := 1;
        while i <= s do
          begin
            HQRNDNormal2(State, U1, U2);
            v[i] := U1;
            if i + 1 <= s then
              begin
                v[i + 1] := U2;
              end;
            i := i + 2;
          end;
        LAMBDA := APVDotProduct(@v[0], 1, s, @v[0], 1, s);
      until AP_FP_NEq(LAMBDA, 0);

      //
      // Prepare and apply reflection
      //
      GenerateReflection(v, s, TAU);
      v[1] := 1;
      ApplyReflectionFromTheRight(a, TAU, v, 0, M - 1, n - s, n - 1, w);
      inc(s);
    end;

  //
  // Second pass.
  //
  i := 0;
  while i <= n - 1 do
    begin
      TAU := 2 * RandomInteger(2) - 1;
      for i_ := 0 to M - 1 do
        begin
          a[i_, i] := TAU * a[i_, i];
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Multiplication of MxN matrix by MxM random Haar distributed orthogonal matrix

  INPUT PARAMETERS:
  A   -   matrix, array[0..M-1, 0..N-1]
  M, N-   matrix size

  OUTPUT PARAMETERS:
  A   -   Q*A, where Q is random MxM orthogonal matrix
  ************************************************************************ *)
procedure RMatrixRndOrthogonalFromTheLeft(var a: TLMatrix; M: TLInt; n: TLInt);
var
  TAU: TLFloat;
  LAMBDA: TLFloat;
  s: TLInt;
  i: TLInt;
  j: TLInt;
  U1: TLFloat;
  U2: TLFloat;
  w: TLVec;
  v: TLVec;
  State: THQRNDState;
begin
  Assert((n >= 1) and (M >= 1),
    'RMatrixRndOrthogonalFromTheRight: N<1 or M<1!');
  if M = 1 then
    begin

      //
      // special case
      //
      TAU := 2 * RandomInteger(2) - 1;
      j := 0;
      while j <= n - 1 do
        begin
          a[0, j] := a[0, j] * TAU;
          inc(j);
        end;
      Exit;
    end;

  //
  // General case.
  // First pass.
  //
  SetLength(w, n (* - 1 + 1 // optimized compiler *) );
  SetLength(v, M + 1);
  HQRNDRandomize(State);
  s := 2;
  while s <= M do
    begin

      //
      // Prepare random normal v
      //
      repeat
        i := 1;
        while i <= s do
          begin
            HQRNDNormal2(State, U1, U2);
            v[i] := U1;
            if i + 1 <= s then
              begin
                v[i + 1] := U2;
              end;
            i := i + 2;
          end;
        LAMBDA := APVDotProduct(@v[0], 1, s, @v[0], 1, s);
      until AP_FP_NEq(LAMBDA, 0);

      //
      // Prepare and apply reflection
      //
      GenerateReflection(v, s, TAU);
      v[1] := 1;
      ApplyReflectionFromTheLeft(a, TAU, v, M - s, M - 1, 0, n - 1, w);
      inc(s);
    end;

  //
  // Second pass.
  //
  i := 0;
  while i <= M - 1 do
    begin
      TAU := 2 * RandomInteger(2) - 1;
      APVMul(@a[i][0], 0, n - 1, TAU);
      inc(i);
    end;
end;

(* ************************************************************************
  Multiplication of MxN TLComplex matrix by NxN random Haar distributed
  TLComplex orthogonal matrix

  INPUT PARAMETERS:
  A   -   matrix, array[0..M-1, 0..N-1]
  M, N-   matrix size

  OUTPUT PARAMETERS:
  A   -   A*Q, where Q is random NxN orthogonal matrix
  ************************************************************************ *)
procedure CMatrixRndOrthogonalFromTheRight(var a: TLComplexMatrix;
  M: TLInt; n: TLInt);
var
  LAMBDA: TLComplex;
  TAU: TLComplex;
  s: TLInt;
  i: TLInt;
  w: TLComplexVec;
  v: TLComplexVec;
  State: THQRNDState;
  i_: TLInt;
begin
  Assert((n >= 1) and (M >= 1),
    'CMatrixRndOrthogonalFromTheRight: N<1 or M<1!');
  if n = 1 then
    begin

      //
      // Special case
      //
      HQRNDRandomize(State);
      HQRNDUnit2(State, TAU.x, TAU.y);
      i := 0;
      while i <= M - 1 do
        begin
          a[i, 0] := C_Mul(a[i, 0], TAU);
          inc(i);
        end;
      Exit;
    end;

  //
  // General case.
  // First pass.
  //
  SetLength(w, M (* - 1 + 1 // optimized compiler *) );
  SetLength(v, n + 1);
  HQRNDRandomize(State);
  s := 2;
  while s <= n do
    begin

      //
      // Prepare random normal v
      //
      repeat
        i := 1;
        while i <= s do
          begin
            HQRNDNormal2(State, TAU.x, TAU.y);
            v[i] := TAU;
            inc(i);
          end;
        LAMBDA := C_Complex(0.0);
        for i_ := 1 to s do
          begin
            LAMBDA := C_Add(LAMBDA, C_Mul(v[i_], Conj(v[i_])));
          end;
      until C_NotEqualR(LAMBDA, 0);

      //
      // Prepare and apply reflection
      //
      ComplexGenerateReflection(v, s, TAU);
      v[1] := C_Complex(1);
      ComplexApplyReflectionFromTheRight(a, TAU, v, 0, M - 1, n - s, n - 1, w);
      inc(s);
    end;

  //
  // Second pass.
  //
  i := 0;
  while i <= n - 1 do
    begin
      HQRNDUnit2(State, TAU.x, TAU.y);
      for i_ := 0 to M - 1 do
        begin
          a[i_, i] := C_Mul(TAU, a[i_, i]);
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Multiplication of MxN TLComplex matrix by MxM random Haar distributed
  TLComplex orthogonal matrix

  INPUT PARAMETERS:
  A   -   matrix, array[0..M-1, 0..N-1]
  M, N-   matrix size

  OUTPUT PARAMETERS:
  A   -   Q*A, where Q is random MxM orthogonal matrix
  ************************************************************************ *)
procedure CMatrixRndOrthogonalFromTheLeft(var a: TLComplexMatrix; M: TLInt; n: TLInt);
var
  TAU: TLComplex;
  LAMBDA: TLComplex;
  s: TLInt;
  i: TLInt;
  j: TLInt;
  w: TLComplexVec;
  v: TLComplexVec;
  State: THQRNDState;
  i_: TLInt;
begin
  Assert((n >= 1) and (M >= 1),
    'CMatrixRndOrthogonalFromTheRight: N<1 or M<1!');
  if M = 1 then
    begin

      //
      // special case
      //
      HQRNDRandomize(State);
      HQRNDUnit2(State, TAU.x, TAU.y);
      j := 0;
      while j <= n - 1 do
        begin
          a[0, j] := C_Mul(a[0, j], TAU);
          inc(j);
        end;
      Exit;
    end;

  //
  // General case.
  // First pass.
  //
  SetLength(w, n (* - 1 + 1 // optimized compiler *) );
  SetLength(v, M + 1);
  HQRNDRandomize(State);
  s := 2;
  while s <= M do
    begin

      //
      // Prepare random normal v
      //
      repeat
        i := 1;
        while i <= s do
          begin
            HQRNDNormal2(State, TAU.x, TAU.y);
            v[i] := TAU;
            inc(i);
          end;
        LAMBDA := C_Complex(0.0);
        for i_ := 1 to s do
          begin
            LAMBDA := C_Add(LAMBDA, C_Mul(v[i_], Conj(v[i_])));
          end;
      until C_NotEqualR(LAMBDA, 0);

      //
      // Prepare and apply reflection
      //
      ComplexGenerateReflection(v, s, TAU);
      v[1] := C_Complex(1);
      ComplexApplyReflectionFromTheLeft(a, TAU, v, M - s, M - 1, 0, n - 1, w);
      inc(s);
    end;

  //
  // Second pass.
  //
  i := 0;
  while i <= M - 1 do
    begin
      HQRNDUnit2(State, TAU.x, TAU.y);
      for i_ := 0 to n - 1 do
        begin
          a[i, i_] := C_Mul(TAU, a[i, i_]);
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Symmetric multiplication of NxN matrix by random Haar distributed
  orthogonal  matrix

  INPUT PARAMETERS:
  A   -   matrix, array[0..N-1, 0..N-1]
  N   -   matrix size

  OUTPUT PARAMETERS:
  A   -   Q'*A*Q, where Q is random NxN orthogonal matrix
  ************************************************************************ *)
procedure SMatrixRndMultiply(var a: TLMatrix; n: TLInt);
var
  TAU: TLFloat;
  LAMBDA: TLFloat;
  s: TLInt;
  i: TLInt;
  U1: TLFloat;
  U2: TLFloat;
  w: TLVec;
  v: TLVec;
  State: THQRNDState;
  i_: TLInt;
begin

  //
  // General case.
  //
  SetLength(w, n (* - 1 + 1 // optimized compiler *) );
  SetLength(v, n + 1);
  HQRNDRandomize(State);
  s := 2;
  while s <= n do
    begin

      //
      // Prepare random normal v
      //
      repeat
        i := 1;
        while i <= s do
          begin
            HQRNDNormal2(State, U1, U2);
            v[i] := U1;
            if i + 1 <= s then
              begin
                v[i + 1] := U2;
              end;
            i := i + 2;
          end;
        LAMBDA := APVDotProduct(@v[0], 1, s, @v[0], 1, s);
      until AP_FP_NEq(LAMBDA, 0);

      //
      // Prepare and apply reflection
      //
      GenerateReflection(v, s, TAU);
      v[1] := 1;
      ApplyReflectionFromTheRight(a, TAU, v, 0, n - 1, n - s, n - 1, w);
      ApplyReflectionFromTheLeft(a, TAU, v, n - s, n - 1, 0, n - 1, w);
      inc(s);
    end;

  //
  // Second pass.
  //
  i := 0;
  while i <= n - 1 do
    begin
      TAU := 2 * RandomInteger(2) - 1;
      for i_ := 0 to n - 1 do
        begin
          a[i_, i] := TAU * a[i_, i];
        end;
      APVMul(@a[i][0], 0, n - 1, TAU);
      inc(i);
    end;
end;

(* ************************************************************************
  Hermitian multiplication of NxN matrix by random Haar distributed
  TLComplex orthogonal matrix

  INPUT PARAMETERS:
  A   -   matrix, array[0..N-1, 0..N-1]
  N   -   matrix size

  OUTPUT PARAMETERS:
  A   -   Q^H*A*Q, where Q is random NxN orthogonal matrix
  ************************************************************************ *)
procedure HMatrixRndMultiply(var a: TLComplexMatrix; n: TLInt);
var
  TAU: TLComplex;
  LAMBDA: TLComplex;
  s: TLInt;
  i: TLInt;
  w: TLComplexVec;
  v: TLComplexVec;
  State: THQRNDState;
  i_: TLInt;
begin

  //
  // General case.
  //
  SetLength(w, n (* - 1 + 1 // optimized compiler *) );
  SetLength(v, n + 1);
  HQRNDRandomize(State);
  s := 2;
  while s <= n do
    begin

      //
      // Prepare random normal v
      //
      repeat
        i := 1;
        while i <= s do
          begin
            HQRNDNormal2(State, TAU.x, TAU.y);
            v[i] := TAU;
            inc(i);
          end;
        LAMBDA := C_Complex(0.0);
        for i_ := 1 to s do
          begin
            LAMBDA := C_Add(LAMBDA, C_Mul(v[i_], Conj(v[i_])));
          end;
      until C_NotEqualR(LAMBDA, 0);

      //
      // Prepare and apply reflection
      //
      ComplexGenerateReflection(v, s, TAU);
      v[1] := C_Complex(1);
      ComplexApplyReflectionFromTheRight(a, TAU, v, 0, n - 1, n - s, n - 1, w);
      ComplexApplyReflectionFromTheLeft(a, Conj(TAU), v, n - s, n - 1, 0,
        n - 1, w);
      inc(s);
    end;

  //
  // Second pass.
  //
  i := 0;
  while i <= n - 1 do
    begin
      HQRNDUnit2(State, TAU.x, TAU.y);
      for i_ := 0 to n - 1 do
        begin
          a[i_, i] := C_Mul(TAU, a[i_, i]);
        end;
      TAU := Conj(TAU);
      for i_ := 0 to n - 1 do
        begin
          a[i, i_] := C_Mul(TAU, a[i, i_]);
        end;
      inc(i);
    end;
end;
