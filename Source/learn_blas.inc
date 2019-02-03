function VectorNorm2(const x: TLVec; const i1, i2: TLInt): TLFloat;
var
  n: TLInt;
  ix: TLInt;
  ABSXI: TLFloat;
  SCL: TLFloat;
  SSQ: TLFloat;
begin
  n := i2 - i1 + 1;
  if n < 1 then
    begin
      Result := 0;
      Exit;
    end;
  if n = 1 then
    begin
      Result := AbsReal(x[i1]);
      Exit;
    end;
  SCL := 0;
  SSQ := 1;
  ix := i1;
  while ix <= i2 do
    begin
      if AP_FP_NEq(x[ix], 0) then
        begin
          ABSXI := AbsReal(x[ix]);
          if AP_FP_Less(SCL, ABSXI) then
            begin
              SSQ := 1 + SSQ * AP_Sqr(SCL / ABSXI);
              SCL := ABSXI;
            end
          else
            begin
              SSQ := SSQ + AP_Sqr(ABSXI / SCL);
            end;
        end;
      inc(ix);
    end;
  Result := SCL * Sqrt(SSQ);
end;

function VectorIdxAbsMax(const x: TLVec; const i1, i2: TLInt): TLInt;
var
  i: TLInt;
  a: TLFloat;
begin
  Result := i1;
  a := AbsReal(x[Result]);
  i := i1 + 1;
  while i <= i2 do
    begin
      if AP_FP_Greater(AbsReal(x[i]), AbsReal(x[Result])) then
        begin
          Result := i;
        end;
      inc(i);
    end;
end;

function ColumnIdxAbsMax(const x: TLMatrix; const i1, i2, j: TLInt): TLInt;
var
  i: TLInt;
  a: TLFloat;
begin
  Result := i1;
  a := AbsReal(x[Result, j]);
  i := i1 + 1;
  while i <= i2 do
    begin
      if AP_FP_Greater(AbsReal(x[i, j]), AbsReal(x[Result, j])) then
        begin
          Result := i;
        end;
      inc(i);
    end;
end;

function RowIdxAbsMax(const x: TLMatrix; const j1, j2, i: TLInt): TLInt;
var
  j: TLInt;
  a: TLFloat;
begin
  Result := j1;
  a := AbsReal(x[i, Result]);
  j := j1 + 1;
  while j <= j2 do
    begin
      if AP_FP_Greater(AbsReal(x[i, j]), AbsReal(x[i, Result])) then
        begin
          Result := j;
        end;
      inc(j);
    end;
end;

function UpperHessenberg1Norm(const a: TLMatrix; const i1, i2, j1, j2: TLInt; var Work: TLVec): TLFloat;
var
  i: TLInt;
  j: TLInt;
begin
  Assert(i2 - i1 = j2 - j1, 'UpperHessenberg1Norm: I2-I1<>J2-J1!');
  j := j1;
  while j <= j2 do
    begin
      Work[j] := 0;
      inc(j);
    end;
  i := i1;
  while i <= i2 do
    begin
      j := Max(j1, j1 + i - i1 - 1);
      while j <= j2 do
        begin
          Work[j] := Work[j] + AbsReal(a[i, j]);
          inc(j);
        end;
      inc(i);
    end;
  Result := 0;
  j := j1;
  while j <= j2 do
    begin
      Result := Max(Result, Work[j]);
      inc(j);
    end;
end;

procedure CopyMatrix(const a: TLMatrix; const IS1, IS2, JS1, JS2: TLInt;
  var b: TLMatrix; const ID1, id2, JD1, JD2: TLInt);
var
  isrc: TLInt;
  idst: TLInt;
begin
  if (IS1 > IS2) or (JS1 > JS2) then
      Exit;
  Assert(IS2 - IS1 = id2 - ID1, 'CopyMatrix: different sizes!');
  Assert(JS2 - JS1 = JD2 - JD1, 'CopyMatrix: different sizes!');
  isrc := IS1;
  while isrc <= IS2 do
    begin
      idst := isrc - IS1 + ID1;
      APVMove(@b[idst][0], JD1, JD2, @a[isrc][0], JS1, JS2);
      inc(isrc);
    end;
end;

procedure InplaceTranspose(var a: TLMatrix; const i1, i2, j1, j2: TLInt; var Work: TLVec);
var
  i: TLInt;
  j: TLInt;
  IPS: TLInt;
  JPS: TLInt;
  L: TLInt;
  i_: TLInt;
  i1_: TLInt;
begin
  if (i1 > i2) or (j1 > j2) then
      Exit;
  Assert(i1 - i2 = j1 - j2, 'InplaceTranspose error: incorrect array size!');
  i := i1;
  while i <= i2 - 1 do
    begin
      j := j1 + i - i1;
      IPS := i + 1;
      JPS := j1 + IPS - i1;
      L := i2 - i;
      i1_ := (IPS) - (1);
      for i_ := 1 to L do
        begin
          Work[i_] := a[i_ + i1_, j];
        end;
      i1_ := (JPS) - (IPS);
      for i_ := IPS to i2 do
        begin
          a[i_, j] := a[i, i_ + i1_];
        end;
      APVMove(@a[i][0], JPS, j2, @Work[0], 1, L);
      inc(i);
    end;
end;

procedure CopyAndTranspose(const a: TLMatrix; IS1, IS2, JS1, JS2: TLInt;
  var b: TLMatrix; ID1, id2, JD1, JD2: TLInt);
var
  isrc: TLInt;
  jdst: TLInt;
  i_: TLInt;
  i1_: TLInt;
begin
  if (IS1 > IS2) or (JS1 > JS2) then
      Exit;
  Assert(IS2 - IS1 = JD2 - JD1, 'CopyAndTranspose: different sizes!');
  Assert(JS2 - JS1 = id2 - ID1, 'CopyAndTranspose: different sizes!');
  isrc := IS1;
  while isrc <= IS2 do
    begin
      jdst := isrc - IS1 + JD1;
      i1_ := (JS1) - (ID1);
      for i_ := ID1 to id2 do
        begin
          b[i_, jdst] := a[isrc, i_ + i1_];
        end;
      inc(isrc);
    end;
end;

procedure MatrixVectorMultiply(const a: TLMatrix; const i1, i2, j1, j2: TLInt; const Trans: Boolean;
  const x: TLVec; const IX1, IX2: TLInt; const alpha: TLFloat;
  var y: TLVec; const IY1, IY2: TLInt; const beta: TLFloat);
var
  i: TLInt;
  v: TLFloat;
begin
  if not Trans then
    begin

      //
      // y := alpha*A*x + beta*y;
      //
      if (i1 > i2) or (j1 > j2) then
          Exit;
      Assert(j2 - j1 = IX2 - IX1, 'MatrixVectorMultiply: A and X dont match!');
      Assert(i2 - i1 = IY2 - IY1, 'MatrixVectorMultiply: A and Y dont match!');

      //
      // beta*y
      //
      if AP_FP_Eq(beta, 0) then
        begin
          i := IY1;
          while i <= IY2 do
            begin
              y[i] := 0;
              inc(i);
            end;
        end
      else
        begin
          APVMul(@y[0], IY1, IY2, beta);
        end;

      //
      // alpha*A*x
      //
      i := i1;
      while i <= i2 do
        begin
          v := APVDotProduct(@a[i][0], j1, j2, @x[0], IX1, IX2);
          y[IY1 + i - i1] := y[IY1 + i - i1] + alpha * v;
          inc(i);
        end;
    end
  else
    begin

      //
      // y := alpha*A'*x + beta*y;
      //
      if (i1 > i2) or (j1 > j2) then
          Exit;
      Assert(i2 - i1 = IX2 - IX1, 'MatrixVectorMultiply: A and X dont match!');
      Assert(j2 - j1 = IY2 - IY1, 'MatrixVectorMultiply: A and Y dont match!');

      //
      // beta*y
      //
      if AP_FP_Eq(beta, 0) then
        begin
          i := IY1;
          while i <= IY2 do
            begin
              y[i] := 0;
              inc(i);
            end;
        end
      else
        begin
          APVMul(@y[0], IY1, IY2, beta);
        end;

      //
      // alpha*A'*x
      //
      i := i1;
      while i <= i2 do
        begin
          v := alpha * x[IX1 + i - i1];
          APVAdd(@y[0], IY1, IY2, @a[i][0], j1, j2, v);
          inc(i);
        end;
    end;
end;

function Pythag2(x: TLFloat; y: TLFloat): TLFloat;
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

procedure MatrixMatrixMultiply(const a: TLMatrix; const AI1, AI2, AJ1, AJ2: TLInt; const TransA: Boolean;
  const b: TLMatrix; const BI1, BI2, BJ1, BJ2: TLInt; const TransB: Boolean;
  const alpha: TLFloat; var c: TLMatrix; const CI1, CI2, CJ1, CJ2: TLInt; const beta: TLFloat; var Work: TLVec);
var
  ARows: TLInt;
  ACols: TLInt;
  BRows: TLInt;
  BCols: TLInt;
  CRows: TLInt;
  CCols: TLInt;
  i: TLInt;
  j: TLInt;
  k: TLInt;
  L: TLInt;
  r: TLInt;
  v: TLFloat;
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
  Assert(ACols = BRows, 'MatrixMatrixMultiply: incorrect matrix sizes!');
  if (ARows <= 0) or (ACols <= 0) or (BRows <= 0) or (BCols <= 0) then
      Exit;
  CRows := ARows;
  CCols := BCols;

  //
  // Test WORK
  //
  i := Max(ARows, ACols);
  i := Max(BRows, i);
  i := Max(i, BCols);
  Work[1] := 0;
  Work[i] := 0;

  //
  // Prepare C
  //
  if AP_FP_Eq(beta, 0) then
    begin
      i := CI1;
      while i <= CI2 do
        begin
          j := CJ1;
          while j <= CJ2 do
            begin
              c[i, j] := 0;
              inc(j);
            end;
          inc(i);
        end;
    end
  else
    begin
      i := CI1;
      while i <= CI2 do
        begin
          APVMul(@c[i][0], CJ1, CJ2, beta);
          inc(i);
        end;
    end;

  //
  // A*B
  //
  if not TransA and not TransB then
    begin
      L := AI1;
      while L <= AI2 do
        begin
          r := BI1;
          while r <= BI2 do
            begin
              v := alpha * a[L, AJ1 + r - BI1];
              k := CI1 + L - AI1;
              APVAdd(@c[k][0], CJ1, CJ2, @b[r][0], BJ1, BJ2, v);
              inc(r);
            end;
          inc(L);
        end;
      Exit;
    end;

  //
  // A*B'
  //
  if not TransA and TransB then
    begin
      if ARows * ACols < BRows * BCols then
        begin
          r := BI1;
          while r <= BI2 do
            begin
              L := AI1;
              while L <= AI2 do
                begin
                  v := APVDotProduct(@a[L][0], AJ1, AJ2, @b[r][0], BJ1, BJ2);
                  c[CI1 + L - AI1, CJ1 + r - BI1] := c[CI1 + L - AI1, CJ1 + r - BI1] + alpha * v;
                  inc(L);
                end;
              inc(r);
            end;
          Exit;
        end
      else
        begin
          L := AI1;
          while L <= AI2 do
            begin
              r := BI1;
              while r <= BI2 do
                begin
                  v := APVDotProduct(@a[L][0], AJ1, AJ2, @b[r][0], BJ1, BJ2);
                  c[CI1 + L - AI1, CJ1 + r - BI1] := c[CI1 + L - AI1, CJ1 + r - BI1] + alpha * v;
                  inc(r);
                end;
              inc(L);
            end;
          Exit;
        end;
    end;

  //
  // A'*B
  //
  if TransA and not TransB then
    begin
      L := AJ1;
      while L <= AJ2 do
        begin
          r := BI1;
          while r <= BI2 do
            begin
              v := alpha * a[AI1 + r - BI1, L];
              k := CI1 + L - AJ1;
              APVAdd(@c[k][0], CJ1, CJ2, @b[r][0], BJ1, BJ2, v);
              inc(r);
            end;
          inc(L);
        end;
      Exit;
    end;

  //
  // A'*B'
  //
  if TransA and TransB then
    begin
      if ARows * ACols < BRows * BCols then
        begin
          r := BI1;
          while r <= BI2 do
            begin
              i := 1;
              while i <= CRows do
                begin
                  Work[i] := 0.0;
                  inc(i);
                end;
              L := AI1;
              while L <= AI2 do
                begin
                  v := alpha * b[r, BJ1 + L - AI1];
                  k := CJ1 + r - BI1;
                  APVAdd(@Work[0], 1, CRows, @a[L][0], AJ1, AJ2, v);
                  inc(L);
                end;
              i1_ := (1) - (CI1);
              for i_ := CI1 to CI2 do
                begin
                  c[i_, k] := c[i_, k] + Work[i_ + i1_];
                end;
              inc(r);
            end;
          Exit;
        end
      else
        begin
          L := AJ1;
          while L <= AJ2 do
            begin
              k := AI2 - AI1 + 1;
              i1_ := (AI1) - (1);
              for i_ := 1 to k do
                begin
                  Work[i_] := a[i_ + i1_, L];
                end;
              r := BI1;
              while r <= BI2 do
                begin
                  v := APVDotProduct(@Work[0], 1, k, @b[r][0], BJ1, BJ2);
                  c[CI1 + L - AJ1, CJ1 + r - BI1] := c[CI1 + L - AJ1, CJ1 + r - BI1] + alpha * v;
                  inc(r);
                end;
              inc(L);
            end;
          Exit;
        end;
    end;
end;
