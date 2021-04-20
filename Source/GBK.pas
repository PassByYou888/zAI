{ ****************************************************************************** }
{ * BASE GBK support,  written by QQ 600585@qq.com                             * }
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
unit GBK;

{$INCLUDE zDefine.inc}

interface

uses DoStatusIO, CoreClasses, PascalStrings, MemoryStream64, ListEngine, UnicodeMixedLib, UPascalStrings;

{ any text fixed }
function GBKString(const s: TUPascalString): TUPascalString;

{ Pinyin encoding conversion, support simplified port body }
function Py(const s: TUPascalString; const multiPy: Boolean): TUPascalString;
function PyNoSpace(const s: TUPascalString): TUPascalString;

{ Simplified to Traditional }
function S2T(const s: TUPascalString): TUPascalString;

{ Simplified to Hongkong Traditional (built-in vocabulary conversion) }
function S2HK(const s: TUPascalString): TUPascalString;

{ Traditional to Simplified (built-in vocabulary conversion) }
function T2S(const s: TUPascalString): TUPascalString;

{ Simplified to Taiwan Traditional (built-in vocabulary conversion) }
function S2TW(const s: TUPascalString): TUPascalString;

implementation

uses FastGBK, GBKMediaCenter;

function GBKChar(const c: TUPascalString): USystemString;
begin
  WaitGBKMediaInit;
{$IFDEF FPC}
  Result := TUPascalString(CharDict.GetDefaultValue(TPascalString(c), TPascalString(c)));
{$ELSE FPC}
  Result := CharDict.GetDefaultValue(c, c);
{$ENDIF FPC}
end;

function GBKString(const s: TUPascalString): TUPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to s.L do
      Result.Append(GBKChar(s[i]));
end;

function PY_Table(const s: TUPascalString; var Successed: Boolean): TUPascalString;
begin
  WaitGBKMediaInit;
  Result := PYDict.GetDefaultValue(s, '');
  Successed := Result.L > 0;
  if Successed then
      Result := Result.ReplaceChar(',', #32)
  else
      Result := s;
end;

function Py_(const s: TUPascalString; const multiPy: Boolean): TUPascalString;
var
  n1, n2, n3: TUPascalString;
  i, j: Integer;
  Successed: Boolean;
begin
  WaitGBKMediaInit;
  n1 := GBKString(s);
  i := 1;

  Result := '';

  while i <= n1.L do
    begin
      Successed := False;
      j := umlMin(PYDict.HashList.MaxNameLen, n1.L - i + 1);
      while j > 1 do
        begin
          n2 := n1.Copy(i, j);
          n3 := PY_Table(n2, Successed);
          if Successed then
            begin
              Result.Append(n3 + #0);
              inc(i, j);
              Break;
            end;
          dec(j);
        end;

      if not Successed then
        begin
          n2 := PY_Table(n1[i], Successed);
          if Successed then
            begin
              if n2.Exists(#32) then
                begin
                  if multiPy then
                      Result.Append('(' + n2.ReplaceChar(#32, ',') + ')' + #0)
                  else
                      Result.Append(umlGetFirstStr(n2.Text, #32).Text + #0);
                end
              else
                  Result.Append(n2 + #0);
            end
          else
            begin
              Result.Append(FastPY(n1[i], multiPy));
            end;
          inc(i);
        end;
    end;
end;

function Py(const s: TUPascalString; const multiPy: Boolean): TUPascalString;
begin
  Result := Py_(s, multiPy);
  if (Result.L > 1) and (Result.Last = #0) then
      Result.DeleteLast;
  Result := Result.ReplaceChar(#0, #32);
end;

function PyNoSpace(const s: TUPascalString): TUPascalString;
var
  n1: TUPascalString;
  c: USystemChar;
  wordFirst, avail: Boolean;
  i: Integer;
begin
  n1 := Py_(s, False);

  wordFirst := True;
  i := 1;
  Result := '';
  avail := True;
  while i <= n1.L do
    begin
      if wordFirst then
        begin
          c := n1.UpperChar[i];
          wordFirst := False;
        end
      else
        begin
          c := n1[i];
          if c = #0 then
            begin
              wordFirst := True;
              avail := False;
              if UCharIn(Result.Last, '1234') then
                  Result.DeleteLast;
            end;
        end;
      if avail then
          Result.Append(c);
      avail := True;
      inc(i);
    end;
end;

function S2T_Table(const s: TUPascalString; var Successed: Boolean): TUPascalString;
begin
  WaitGBKMediaInit;
  Result := s2tDict.GetDefaultValue(s, '');
  Successed := Result.L > 0;
  if not Successed then
      Result := s;
end;

function S2T(const s: TUPascalString): TUPascalString;
var
  n1, n2, n3: TUPascalString;
  i, j: Integer;
  Successed: Boolean;
begin
  WaitGBKMediaInit;
  n1 := GBKString(s);
  i := 1;

  Result := '';

  while i <= n1.L do
    begin
      Successed := False;
      j := umlMin(s2tDict.HashList.MaxNameLen, n1.L - i + 1);
      while j > 1 do
        begin
          n2 := n1.Copy(i, j);
          n3 := S2T_Table(n2, Successed);
          if Successed then
            begin
              Result.Append(n3);
              inc(i, j);
              Break;
            end;
          dec(j);
        end;

      if not Successed then
        begin
          n2 := S2T_Table(n1[i], Successed);
          if Successed then
              Result.Append(n2)
          else
              Result.Append(n1[i]);
          inc(i);
        end;
    end;
end;

function t2HK_Table(const s: TUPascalString; var Successed: Boolean): TUPascalString;
begin
  WaitGBKMediaInit;
  Result := t2hkDict.GetDefaultValue(s, '');
  Successed := Result.L > 0;
  if not Successed then
      Result := s;
end;

function S2HK(const s: TUPascalString): TUPascalString;
var
  n1, n2, n3: TUPascalString;
  i, j: Integer;
  Successed: Boolean;
begin
  WaitGBKMediaInit;
  n1 := S2T(s);
  i := 1;

  Result := '';

  while i <= n1.L do
    begin
      Successed := False;
      j := umlMin(t2hkDict.HashList.MaxNameLen, n1.L - i + 1);
      while j > 1 do
        begin
          n2 := n1.Copy(i, j);
          n3 := t2HK_Table(n2, Successed);
          if Successed then
            begin
              Result.Append(n3);
              inc(i, j);
              Break;
            end;
          dec(j);
        end;

      if not Successed then
        begin
          n2 := t2HK_Table(n1[i], Successed);
          if Successed then
              Result.Append(n2)
          else
              Result.Append(n1[i]);
          inc(i);
        end;
    end;
end;

function t2s_Table(const s: TUPascalString; var Successed: Boolean): TUPascalString;
begin
  WaitGBKMediaInit;
  Result := t2sDict.GetDefaultValue(s, '');
  Successed := Result.L > 0;
  if not Successed then
      Result := s;
end;

function T2S(const s: TUPascalString): TUPascalString;
var
  n1, n2, n3: TUPascalString;
  i, j: Integer;
  Successed: Boolean;
begin
  WaitGBKMediaInit;
  n1 := s;
  i := 1;

  Result := '';

  while i <= n1.L do
    begin
      Successed := False;
      j := umlMin(t2sDict.HashList.MaxNameLen, n1.L - i + 1);
      while j > 1 do
        begin
          n2 := n1.Copy(i, j);
          n3 := t2s_Table(n2, Successed);
          if Successed then
            begin
              Result.Append(n3);
              inc(i, j);
              Break;
            end;
          dec(j);
        end;

      if not Successed then
        begin
          n2 := t2s_Table(n1[i], Successed);
          if Successed then
              Result.Append(n2)
          else
              Result.Append(n1[i]);
          inc(i);
        end;
    end;

  Result := GBKString(Result);
end;

function t2tw_Table(const s: TUPascalString; var Successed: Boolean): TUPascalString;
begin
  WaitGBKMediaInit;
  Result := t2twDict.GetDefaultValue(s, '');
  Successed := Result.L > 0;
  if not Successed then
      Result := s;
end;

function S2TW(const s: TUPascalString): TUPascalString;
var
  n1, n2, n3: TUPascalString;
  i, j: Integer;
  Successed: Boolean;
begin
  WaitGBKMediaInit;
  n1 := S2T(s);
  i := 1;

  Result := '';

  while i <= n1.L do
    begin
      Successed := False;
      j := umlMin(t2twDict.HashList.MaxNameLen, n1.L - i + 1);
      while j > 1 do
        begin
          n2 := n1.Copy(i, j);
          n3 := t2tw_Table(n2, Successed);
          if Successed then
            begin
              Result.Append(n3);
              inc(i, j);
              Break;
            end;
          dec(j);
        end;

      if not Successed then
        begin
          n2 := t2tw_Table(n1[i], Successed);
          if Successed then
              Result.Append(n2)
          else
              Result.Append(n1[i]);
          inc(i);
        end;
    end;
end;

end.
