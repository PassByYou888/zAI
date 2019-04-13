{ ****************************************************************************** }
{ * matrix safe support, by QQ 600585@qq.com                                   * }
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


function CBasicSolveAndUpdate(alpha: TLComplex; beta: TLComplex; LnMax: TLFloat;
  BNORM: TLFloat; MaxGrowth: TLFloat; var XNORM: TLFloat;
  var x: TLComplex): Boolean; forward;


(* ************************************************************************
  Real implementation
  of CMatrixScaledTRSafeSolve
  ************************************************************************ *)
function RMatrixScaledTRSafeSolve(const a: TLMatrix; SA: TLFloat;
  n: TLInt; var x: TLVec; IsUpper: Boolean; Trans: TLInt;
  IsUnit: Boolean; MaxGrowth: TLFloat): Boolean;
var
  LnMax: TLFloat;
  NrmB : TLFloat;
  NrmX : TLFloat;
  i    : TLInt;
  alpha: TLComplex;
  beta : TLComplex;
  vr   : TLFloat;
  Cx   : TLComplex;
  tmp  : TLVec;
begin
  Assert(n > 0, 'RMatrixTRSafeSolve: incorrect N!');
  Assert((Trans = 0) or (Trans = 1), 'RMatrixTRSafeSolve: incorrect Trans!');
  Result := True;
  LnMax := ln(MaxRealNumber);

  //
  // Quick return if possible
  //
  if n <= 0 then
    begin
      Exit;
    end;

  //
  // Load norms: right part and X
  //
  NrmB := 0;
  i := 0;
  while i <= n - 1 do
    begin
      NrmB := Max(NrmB, AbsReal(x[i]));
      inc(i);
    end;
  NrmX := 0;

  //
  // Solve
  //
  SetLength(tmp, n);
  Result := True;
  if IsUpper and (Trans = 0) then
    begin

      //
      // U*x = b
      //
      i := n - 1;
      while i >= 0 do
        begin

          //
          // Task is reduced to alpha*x[i] = beta
          //
          if IsUnit then
            begin
              alpha := C_Complex(SA);
            end
          else
            begin
              alpha := C_Complex(a[i, i] * SA);
            end;
          if i < n - 1 then
            begin
              APVMove(@tmp[0], i + 1, n - 1, @a[i][0], i + 1, n - 1, SA);
              vr := APVDotProduct(@tmp[0], i + 1, n - 1, @x[0], i + 1, n - 1);
              beta := C_Complex(x[i] - vr);
            end
          else
            begin
              beta := C_Complex(x[i]);
            end;

          //
          // solve alpha*x[i] = beta
          //
          Result := CBasicSolveAndUpdate(alpha, beta, LnMax, NrmB, MaxGrowth,
            NrmX, Cx);
          if not Result then
            begin
              Exit;
            end;
          x[i] := Cx.x;
          dec(i);
        end;
      Exit;
    end;
  if not IsUpper and (Trans = 0) then
    begin

      //
      // L*x = b
      //
      i := 0;
      while i <= n - 1 do
        begin

          //
          // Task is reduced to alpha*x[i] = beta
          //
          if IsUnit then
            begin
              alpha := C_Complex(SA);
            end
          else
            begin
              alpha := C_Complex(a[i, i] * SA);
            end;
          if i > 0 then
            begin
              APVMove(@tmp[0], 0, i - 1, @a[i][0], 0, i - 1, SA);
              vr := APVDotProduct(@tmp[0], 0, i - 1, @x[0], 0, i - 1);
              beta := C_Complex(x[i] - vr);
            end
          else
            begin
              beta := C_Complex(x[i]);
            end;

          //
          // solve alpha*x[i] = beta
          //
          Result := CBasicSolveAndUpdate(alpha, beta, LnMax, NrmB, MaxGrowth,
            NrmX, Cx);
          if not Result then
            begin
              Exit;
            end;
          x[i] := Cx.x;
          inc(i);
        end;
      Exit;
    end;
  if IsUpper and (Trans = 1) then
    begin

      //
      // U^T*x = b
      //
      i := 0;
      while i <= n - 1 do
        begin

          //
          // Task is reduced to alpha*x[i] = beta
          //
          if IsUnit then
            begin
              alpha := C_Complex(SA);
            end
          else
            begin
              alpha := C_Complex(a[i, i] * SA);
            end;
          beta := C_Complex(x[i]);

          //
          // solve alpha*x[i] = beta
          //
          Result := CBasicSolveAndUpdate(alpha, beta, LnMax, NrmB, MaxGrowth,
            NrmX, Cx);
          if not Result then
            begin
              Exit;
            end;
          x[i] := Cx.x;

          //
          // update the rest of right part
          //
          if i < n - 1 then
            begin
              vr := Cx.x;
              APVMove(@tmp[0], i + 1, n - 1, @a[i][0], i + 1, n - 1, SA);
              APVSub(@x[0], i + 1, n - 1, @tmp[0], i + 1, n - 1, vr);
            end;
          inc(i);
        end;
      Exit;
    end;
  if not IsUpper and (Trans = 1) then
    begin

      //
      // L^T*x = b
      //
      i := n - 1;
      while i >= 0 do
        begin

          //
          // Task is reduced to alpha*x[i] = beta
          //
          if IsUnit then
            begin
              alpha := C_Complex(SA);
            end
          else
            begin
              alpha := C_Complex(a[i, i] * SA);
            end;
          beta := C_Complex(x[i]);

          //
          // solve alpha*x[i] = beta
          //
          Result := CBasicSolveAndUpdate(alpha, beta, LnMax, NrmB, MaxGrowth,
            NrmX, Cx);
          if not Result then
            begin
              Exit;
            end;
          x[i] := Cx.x;

          //
          // update the rest of right part
          //
          if i > 0 then
            begin
              vr := Cx.x;
              APVMove(@tmp[0], 0, i - 1, @a[i][0], 0, i - 1, SA);
              APVSub(@x[0], 0, i - 1, @tmp[0], 0, i - 1, vr);
            end;
          dec(i);
        end;
      Exit;
    end;
  Result := False;
end;

(* ************************************************************************
  Internal subroutine for safe solution of

  SA*op(A)=b

  where  A  is  NxN  upper/lower  triangular/unitriangular  matrix, op(A) is
  either identity transform, transposition or Hermitian transposition, SA is
  a scaling factor such that max(|SA*A[i,j]|) is close to 1.0 in magnutude.

  This subroutine  limits  relative  growth  of  solution  (in inf-norm)  by
  MaxGrowth,  returning  False  if  growth  exceeds MaxGrowth. Degenerate or
  near-degenerate matrices are handled correctly (False is returned) as long
  as MaxGrowth is significantly less than MaxRealNumber/norm(b).
  ************************************************************************ *)
function CMatrixScaledTRSafeSolve(const a: TLComplexMatrix; SA: TLFloat;
  n: TLInt; var x: TLComplexVec; IsUpper: Boolean;
  Trans: TLInt; IsUnit: Boolean; MaxGrowth: TLFloat): Boolean;
var
  LnMax: TLFloat;
  NrmB : TLFloat;
  NrmX : TLFloat;
  i    : TLInt;
  alpha: TLComplex;
  beta : TLComplex;
  VC   : TLComplex;
  tmp  : TLComplexVec;
  i_   : TLInt;
begin
  Assert(n > 0, 'CMatrixTRSafeSolve: incorrect N!');
  Assert((Trans = 0) or (Trans = 1) or (Trans = 2),
    'CMatrixTRSafeSolve: incorrect Trans!');
  Result := True;
  LnMax := ln(MaxRealNumber);

  //
  // Quick return if possible
  //
  if n <= 0 then
    begin
      Exit;
    end;

  //
  // Load norms: right part and X
  //
  NrmB := 0;
  i := 0;
  while i <= n - 1 do
    begin
      NrmB := Max(NrmB, AbsComplex(x[i]));
      inc(i);
    end;
  NrmX := 0;

  //
  // Solve
  //
  SetLength(tmp, n);
  Result := True;
  if IsUpper and (Trans = 0) then
    begin

      //
      // U*x = b
      //
      i := n - 1;
      while i >= 0 do
        begin

          //
          // Task is reduced to alpha*x[i] = beta
          //
          if IsUnit then
            begin
              alpha := C_Complex(SA);
            end
          else
            begin
              alpha := C_MulR(a[i, i], SA);
            end;
          if i < n - 1 then
            begin
              for i_ := i + 1 to n - 1 do
                begin
                  tmp[i_] := C_MulR(a[i, i_], SA);
                end;
              VC := C_Complex(0.0);
              for i_ := i + 1 to n - 1 do
                begin
                  VC := C_Add(VC, C_Mul(tmp[i_], x[i_]));
                end;
              beta := C_Sub(x[i], VC);
            end
          else
            begin
              beta := x[i];
            end;

          //
          // solve alpha*x[i] = beta
          //
          Result := CBasicSolveAndUpdate(alpha, beta, LnMax, NrmB, MaxGrowth,
            NrmX, VC);
          if not Result then
            begin
              Exit;
            end;
          x[i] := VC;
          dec(i);
        end;
      Exit;
    end;
  if not IsUpper and (Trans = 0) then
    begin

      //
      // L*x = b
      //
      i := 0;
      while i <= n - 1 do
        begin

          //
          // Task is reduced to alpha*x[i] = beta
          //
          if IsUnit then
            begin
              alpha := C_Complex(SA);
            end
          else
            begin
              alpha := C_MulR(a[i, i], SA);
            end;
          if i > 0 then
            begin
              for i_ := 0 to i - 1 do
                begin
                  tmp[i_] := C_MulR(a[i, i_], SA);
                end;
              VC := C_Complex(0.0);
              for i_ := 0 to i - 1 do
                begin
                  VC := C_Add(VC, C_Mul(tmp[i_], x[i_]));
                end;
              beta := C_Sub(x[i], VC);
            end
          else
            begin
              beta := x[i];
            end;

          //
          // solve alpha*x[i] = beta
          //
          Result := CBasicSolveAndUpdate(alpha, beta, LnMax, NrmB, MaxGrowth,
            NrmX, VC);
          if not Result then
            begin
              Exit;
            end;
          x[i] := VC;
          inc(i);
        end;
      Exit;
    end;
  if IsUpper and (Trans = 1) then
    begin

      //
      // U^T*x = b
      //
      i := 0;
      while i <= n - 1 do
        begin

          //
          // Task is reduced to alpha*x[i] = beta
          //
          if IsUnit then
            begin
              alpha := C_Complex(SA);
            end
          else
            begin
              alpha := C_MulR(a[i, i], SA);
            end;
          beta := x[i];

          //
          // solve alpha*x[i] = beta
          //
          Result := CBasicSolveAndUpdate(alpha, beta, LnMax, NrmB, MaxGrowth,
            NrmX, VC);
          if not Result then
            begin
              Exit;
            end;
          x[i] := VC;

          //
          // update the rest of right part
          //
          if i < n - 1 then
            begin
              for i_ := i + 1 to n - 1 do
                begin
                  tmp[i_] := C_MulR(a[i, i_], SA);
                end;
              for i_ := i + 1 to n - 1 do
                begin
                  x[i_] := C_Sub(x[i_], C_Mul(VC, tmp[i_]));
                end;
            end;
          inc(i);
        end;
      Exit;
    end;
  if not IsUpper and (Trans = 1) then
    begin

      //
      // L^T*x = b
      //
      i := n - 1;
      while i >= 0 do
        begin

          //
          // Task is reduced to alpha*x[i] = beta
          //
          if IsUnit then
            begin
              alpha := C_Complex(SA);
            end
          else
            begin
              alpha := C_MulR(a[i, i], SA);
            end;
          beta := x[i];

          //
          // solve alpha*x[i] = beta
          //
          Result := CBasicSolveAndUpdate(alpha, beta, LnMax, NrmB, MaxGrowth,
            NrmX, VC);
          if not Result then
            begin
              Exit;
            end;
          x[i] := VC;

          //
          // update the rest of right part
          //
          if i > 0 then
            begin
              for i_ := 0 to i - 1 do
                begin
                  tmp[i_] := C_MulR(a[i, i_], SA);
                end;
              for i_ := 0 to i - 1 do
                begin
                  x[i_] := C_Sub(x[i_], C_Mul(VC, tmp[i_]));
                end;
            end;
          dec(i);
        end;
      Exit;
    end;
  if IsUpper and (Trans = 2) then
    begin

      //
      // U^H*x = b
      //
      i := 0;
      while i <= n - 1 do
        begin

          //
          // Task is reduced to alpha*x[i] = beta
          //
          if IsUnit then
            begin
              alpha := C_Complex(SA);
            end
          else
            begin
              alpha := C_MulR(Conj(a[i, i]), SA);
            end;
          beta := x[i];

          //
          // solve alpha*x[i] = beta
          //
          Result := CBasicSolveAndUpdate(alpha, beta, LnMax, NrmB, MaxGrowth,
            NrmX, VC);
          if not Result then
            begin
              Exit;
            end;
          x[i] := VC;

          //
          // update the rest of right part
          //
          if i < n - 1 then
            begin
              for i_ := i + 1 to n - 1 do
                begin
                  tmp[i_] := C_MulR(Conj(a[i, i_]), SA);
                end;
              for i_ := i + 1 to n - 1 do
                begin
                  x[i_] := C_Sub(x[i_], C_Mul(VC, tmp[i_]));
                end;
            end;
          inc(i);
        end;
      Exit;
    end;
  if not IsUpper and (Trans = 2) then
    begin

      //
      // L^T*x = b
      //
      i := n - 1;
      while i >= 0 do
        begin

          //
          // Task is reduced to alpha*x[i] = beta
          //
          if IsUnit then
            begin
              alpha := C_Complex(SA);
            end
          else
            begin
              alpha := C_MulR(Conj(a[i, i]), SA);
            end;
          beta := x[i];

          //
          // solve alpha*x[i] = beta
          //
          Result := CBasicSolveAndUpdate(alpha, beta, LnMax, NrmB, MaxGrowth,
            NrmX, VC);
          if not Result then
            begin
              Exit;
            end;
          x[i] := VC;

          //
          // update the rest of right part
          //
          if i > 0 then
            begin
              for i_ := 0 to i - 1 do
                begin
                  tmp[i_] := C_MulR(Conj(a[i, i_]), SA);
                end;
              for i_ := 0 to i - 1 do
                begin
                  x[i_] := C_Sub(x[i_], C_Mul(VC, tmp[i_]));
                end;
            end;
          dec(i);
        end;
      Exit;
    end;
  Result := False;
end;

(* ************************************************************************
  TLComplex basic solver-updater for reduced linear system

  alpha*x[i] = beta

  solves this equation and updates it in overlfow-safe manner (keeping track
  of relative growth of solution).

  Parameters:
  Alpha   -   alpha
  Beta    -   beta
  LnMax   -   precomputed Ln(MaxRealNumber)
  BNorm   -   inf-norm of b (right part of original system)
  MaxGrowth-  maximum growth of norm(x) relative to norm(b)
  XNorm   -   inf-norm of other components of X (which are already processed)
  it is updated by CBasicSolveAndUpdate.
  X       -   solution
  ************************************************************************ *)
function CBasicSolveAndUpdate(alpha: TLComplex; beta: TLComplex; LnMax: TLFloat;
  BNORM: TLFloat; MaxGrowth: TLFloat; var XNORM: TLFloat;
  var x: TLComplex): Boolean;
var
  v: TLFloat;
begin
  Result := False;
  if C_EqualR(alpha, 0) then
    begin
      Exit;
    end;
  if C_NotEqualR(beta, 0) then
    begin

      //
      // alpha*x[i]=beta
      //
      v := ln(AbsComplex(beta)) - ln(AbsComplex(alpha));
      if AP_FP_Greater(v, LnMax) then
        begin
          Exit;
        end;
      x := C_Div(beta, alpha);
    end
  else
    begin

      //
      // alpha*x[i]=0
      //
      x := C_Complex(0);
    end;

  //
  // update NrmX, test growth limit
  //
  XNORM := Max(XNORM, AbsComplex(x));
  if AP_FP_Greater(XNORM, MaxGrowth * BNORM) then
    begin
      Exit;
    end;
  Result := True;
end; 
 
 
