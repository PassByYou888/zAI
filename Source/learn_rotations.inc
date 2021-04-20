{ ****************************************************************************** }
{ * matrix rotations support, by QQ 600585@qq.com                              * }
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
  Application of a sequence of elementary rotations to a matrix

  The algorithm pre-multiplies the matrix by a sequence of rotation
  transformations which is given by arrays C and S. Depending on the value
  of the IsForward parameter either 1 and 2, 3 and 4 and so on (if IsForward=true)
  rows are rotated, or the rows N and N-1, N-2 and N-3 and so on, are rotated.

  Not the whole matrix but only a part of it is transformed (rows from M1 to
  M2, columns from N1 to N2). Only the elements of this submatrix are changed.

  Input parameters:
  IsForward   -   the sequence of the rotation application.
  M1,M2       -   the range of rows to be transformed.
  N1, N2      -   the range of columns to be transformed.
  C,S         -   transformation coefficients.
  Array whose index ranges within [1..M2-M1].
  A           -   processed matrix.
  WORK        -   working array whose index ranges within [N1..N2].

  Output parameters:
  A           -   transformed matrix.
  ************************************************************************ *)
procedure ApplyRotationsFromTheLeft(IsForward: Boolean; m1: TLInt; m2: TLInt; n1: TLInt; n2: TLInt;
  const c: TLVec; const s: TLVec; var a: TLMatrix; var Work: TLVec);
var
  j    : TLInt;
  JP1  : TLInt;
  CTemp: TLFloat;
  STEMP: TLFloat;
  Temp : TLFloat;
begin
  if (m1 > m2) or (n1 > n2) then
    begin
      Exit;
    end;

  //
  // Form  P * A
  //
  if IsForward then
    begin
      if n1 <> n2 then
        begin

          //
          // Common case: N1<>N2
          //
          j := m1;
          while j <= m2 - 1 do
            begin
              CTemp := c[j - m1 + 1];
              STEMP := s[j - m1 + 1];
              if AP_FP_NEq(CTemp, 1) or AP_FP_NEq(STEMP, 0) then
                begin
                  JP1 := j + 1;
                  APVMove(@Work[0], n1, n2, @a[JP1][0], n1, n2, CTemp);
                  APVSub(@Work[0], n1, n2, @a[j][0], n1, n2, STEMP);
                  APVMul(@a[j][0], n1, n2, CTemp);
                  APVAdd(@a[j][0], n1, n2, @a[JP1][0], n1, n2, STEMP);
                  APVMove(@a[JP1][0], n1, n2, @Work[0], n1, n2);
                end;
              inc(j);
            end;
        end
      else
        begin

          //
          // Special case: N1=N2
          //
          j := m1;
          while j <= m2 - 1 do
            begin
              CTemp := c[j - m1 + 1];
              STEMP := s[j - m1 + 1];
              if AP_FP_NEq(CTemp, 1) or AP_FP_NEq(STEMP, 0) then
                begin
                  Temp := a[j + 1, n1];
                  a[j + 1, n1] := CTemp * Temp - STEMP * a[j, n1];
                  a[j, n1] := STEMP * Temp + CTemp * a[j, n1];
                end;
              inc(j);
            end;
        end;
    end
  else
    begin
      if n1 <> n2 then
        begin

          //
          // Common case: N1<>N2
          //
          j := m2 - 1;
          while j >= m1 do
            begin
              CTemp := c[j - m1 + 1];
              STEMP := s[j - m1 + 1];
              if AP_FP_NEq(CTemp, 1) or AP_FP_NEq(STEMP, 0) then
                begin
                  JP1 := j + 1;
                  APVMove(@Work[0], n1, n2, @a[JP1][0], n1, n2, CTemp);
                  APVSub(@Work[0], n1, n2, @a[j][0], n1, n2, STEMP);
                  APVMul(@a[j][0], n1, n2, CTemp);
                  APVAdd(@a[j][0], n1, n2, @a[JP1][0], n1, n2, STEMP);
                  APVMove(@a[JP1][0], n1, n2, @Work[0], n1, n2);
                end;
              dec(j);
            end;
        end
      else
        begin

          //
          // Special case: N1=N2
          //
          j := m2 - 1;
          while j >= m1 do
            begin
              CTemp := c[j - m1 + 1];
              STEMP := s[j - m1 + 1];
              if AP_FP_NEq(CTemp, 1) or AP_FP_NEq(STEMP, 0) then
                begin
                  Temp := a[j + 1, n1];
                  a[j + 1, n1] := CTemp * Temp - STEMP * a[j, n1];
                  a[j, n1] := STEMP * Temp + CTemp * a[j, n1];
                end;
              dec(j);
            end;
        end;
    end;
end;

(* ************************************************************************
  Application of a sequence of elementary rotations to a matrix

  The algorithm post-multiplies the matrix by a sequence of rotation
  transformations which is given by arrays C and S. Depending on the value
  of the IsForward parameter either 1 and 2, 3 and 4 and so on (if IsForward=true)
  rows are rotated, or the rows N and N-1, N-2 and N-3 and so on are rotated.

  Not the whole matrix but only a part of it is transformed (rows from M1
  to M2, columns from N1 to N2). Only the elements of this submatrix are changed.

  Input parameters:
  IsForward   -   the sequence of the rotation application.
  M1,M2       -   the range of rows to be transformed.
  N1, N2      -   the range of columns to be transformed.
  C,S         -   transformation coefficients.
  Array whose index ranges within [1..N2-N1].
  A           -   processed matrix.
  WORK        -   working array whose index ranges within [M1..M2].

  Output parameters:
  A           -   transformed matrix.
  ************************************************************************ *)
procedure ApplyRotationsFromTheRight(IsForward: Boolean; m1: TLInt; m2: TLInt; n1: TLInt; n2: TLInt;
  const c: TLVec; const s: TLVec; var a: TLMatrix; var Work: TLVec);
var
  j    : TLInt;
  JP1  : TLInt;
  CTemp: TLFloat;
  STEMP: TLFloat;
  Temp : TLFloat;
  i_   : TLInt;
begin

  //
  // Form A * P'
  //
  if IsForward then
    begin
      if m1 <> m2 then
        begin

          //
          // Common case: M1<>M2
          //
          j := n1;
          while j <= n2 - 1 do
            begin
              CTemp := c[j - n1 + 1];
              STEMP := s[j - n1 + 1];
              if AP_FP_NEq(CTemp, 1) or AP_FP_NEq(STEMP, 0) then
                begin
                  JP1 := j + 1;
                  for i_ := m1 to m2 do
                    begin
                      Work[i_] := CTemp * a[i_, JP1];
                    end;
                  for i_ := m1 to m2 do
                    begin
                      Work[i_] := Work[i_] - STEMP * a[i_, j];
                    end;
                  for i_ := m1 to m2 do
                    begin
                      a[i_, j] := CTemp * a[i_, j];
                    end;
                  for i_ := m1 to m2 do
                    begin
                      a[i_, j] := a[i_, j] + STEMP * a[i_, JP1];
                    end;
                  for i_ := m1 to m2 do
                    begin
                      a[i_, JP1] := Work[i_];
                    end;
                end;
              inc(j);
            end;
        end
      else
        begin

          //
          // Special case: M1=M2
          //
          j := n1;
          while j <= n2 - 1 do
            begin
              CTemp := c[j - n1 + 1];
              STEMP := s[j - n1 + 1];
              if AP_FP_NEq(CTemp, 1) or AP_FP_NEq(STEMP, 0) then
                begin
                  Temp := a[m1, j + 1];
                  a[m1, j + 1] := CTemp * Temp - STEMP * a[m1, j];
                  a[m1, j] := STEMP * Temp + CTemp * a[m1, j];
                end;
              inc(j);
            end;
        end;
    end
  else
    begin
      if m1 <> m2 then
        begin

          //
          // Common case: M1<>M2
          //
          j := n2 - 1;
          while j >= n1 do
            begin
              CTemp := c[j - n1 + 1];
              STEMP := s[j - n1 + 1];
              if AP_FP_NEq(CTemp, 1) or AP_FP_NEq(STEMP, 0) then
                begin
                  JP1 := j + 1;
                  for i_ := m1 to m2 do
                    begin
                      Work[i_] := CTemp * a[i_, JP1];
                    end;
                  for i_ := m1 to m2 do
                    begin
                      Work[i_] := Work[i_] - STEMP * a[i_, j];
                    end;
                  for i_ := m1 to m2 do
                    begin
                      a[i_, j] := CTemp * a[i_, j];
                    end;
                  for i_ := m1 to m2 do
                    begin
                      a[i_, j] := a[i_, j] + STEMP * a[i_, JP1];
                    end;
                  for i_ := m1 to m2 do
                    begin
                      a[i_, JP1] := Work[i_];
                    end;
                end;
              dec(j);
            end;
        end
      else
        begin

          //
          // Special case: M1=M2
          //
          j := n2 - 1;
          while j >= n1 do
            begin
              CTemp := c[j - n1 + 1];
              STEMP := s[j - n1 + 1];
              if AP_FP_NEq(CTemp, 1) or AP_FP_NEq(STEMP, 0) then
                begin
                  Temp := a[m1, j + 1];
                  a[m1, j + 1] := CTemp * Temp - STEMP * a[m1, j];
                  a[m1, j] := STEMP * Temp + CTemp * a[m1, j];
                end;
              dec(j);
            end;
        end;
    end;
end;

(* ************************************************************************
  The subroutine generates the elementary rotation, so that:

  [  CS  SN  ]  .  [ F ]  =  [ R ]
  [ -SN  CS  ]     [ G ]     [ 0 ]

  CS**2 + SN**2 = 1
  ************************************************************************ *)
procedure GenerateRotation(f: TLFloat; g: TLFloat; var cs: TLFloat; var sn: TLFloat; var r: TLFloat);
var
  f1: TLFloat;
  G1: TLFloat;
begin
  if AP_FP_Eq(g, 0) then
    begin
      cs := 1;
      sn := 0;
      r := f;
    end
  else
    begin
      if AP_FP_Eq(f, 0) then
        begin
          cs := 0;
          sn := 1;
          r := g;
        end
      else
        begin
          f1 := f;
          G1 := g;
          if AP_FP_Greater(AbsReal(f1), AbsReal(G1)) then
            begin
              r := AbsReal(f1) * Sqrt(1 + AP_Sqr(G1 / f1));
            end
          else
            begin
              r := AbsReal(G1) * Sqrt(1 + AP_Sqr(f1 / G1));
            end;
          cs := f1 / r;
          sn := G1 / r;
          if AP_FP_Greater(AbsReal(f), AbsReal(g)) and AP_FP_Less(cs, 0) then
            begin
              cs := -cs;
              sn := -sn;
              r := -r;
            end;
        end;
    end;
end; 
 
 
