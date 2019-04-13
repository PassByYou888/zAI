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

uses DoStatusIO, CoreClasses, PascalStrings, MemoryStream64, ListEngine, UnicodeMixedLib,
  UPascalStrings;

{ any text fixed }
function GBKString(const s: TUPascalString): TUPascalString;

{ Pinyin encoding conversion, support simplified port body }
function Py(const s: TUPascalString; const multiPy: Boolean): TUPascalString;

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
  for i := 1 to s.Len do
      Result.Append(GBKChar(s[i]));
end;

function PY_Table(const s: TUPascalString; var Successed: Boolean): TUPascalString;
begin
  Result := PYDict.GetDefaultValue(s, '');
  Successed := Result.Len > 0;
  if Successed then
      Result := Result.ReplaceChar(',', #32)
  else
      Result := s;
end;

function Py(const s: TUPascalString; const multiPy: Boolean): TUPascalString;
var
  ns, n2, n3: TUPascalString;
  i, j: Integer;
  Successed: Boolean;
begin
  ns := GBKString(s);
  i := 1;

  Result := '';

  while i <= ns.Len do
    begin
      Successed := False;
      j := umlMin(PYDict.HashList.MaxNameLen, ns.Len - i);
      while j > 1 do
        begin
          n2 := ns.Copy(i, j);
          n3 := PY_Table(n2, Successed);
          if Successed then
            begin
              Result.Append(n3 + #32);
              inc(i, j);
              Break;
            end;
          dec(j);
        end;

      if not Successed then
        begin
          n2 := PY_Table(ns[i], Successed);
          if Successed then
            begin
              if n2.Exists(#32) then
                begin
                  if multiPy then
                      Result.Append('(' + n2.ReplaceChar(#32, ',') + ')' + #32)
                  else
                      Result.Append(umlGetFirstStr(n2.Text, #32).Text + #32);
                end
              else
                  Result.Append(n2 + #32);
            end
          else
            begin
              if (Result.Len > 1) and (Result.Last = #32) and (CharIn(ns[i], ' {}[]\|:";'#39'<>?,./~!@#$%^&*()-=_+')) then
                  Result.DeleteLast;

              Result.Append(FastPY(ns[i], multiPy));
            end;
          inc(i);
        end;
    end;
  if (Result.Len > 1) and (Result.Last = #32) then
      Result.DeleteLast;
end;

function S2T_Table(const s: TUPascalString; var Successed: Boolean): TUPascalString;
begin
  Result := s2tDict.GetDefaultValue(s, '');
  Successed := Result.Len > 0;
  if not Successed then
      Result := s;
end;

function S2T(const s: TUPascalString): TUPascalString;
var
  ns, n2, n3: TUPascalString;
  i, j: Integer;
  Successed: Boolean;
begin
  ns := GBKString(s);
  i := 1;

  Result := '';

  while i <= ns.Len do
    begin
      Successed := False;
      j := umlMin(s2tDict.HashList.MaxNameLen, ns.Len - i);
      while j > 1 do
        begin
          n2 := ns.Copy(i, j);
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
          n2 := S2T_Table(ns[i], Successed);
          if Successed then
              Result.Append(n2)
          else
              Result.Append(ns[i]);
          inc(i);
        end;
    end;
end;

function t2HK_Table(const s: TUPascalString; var Successed: Boolean): TUPascalString;
begin
  Result := t2hkDict.GetDefaultValue(s, '');
  Successed := Result.Len > 0;
  if not Successed then
      Result := s;
end;

function S2HK(const s: TUPascalString): TUPascalString;
var
  ns, n2, n3: TUPascalString;
  i, j: Integer;
  Successed: Boolean;
begin
  ns := S2T(s);
  i := 1;

  Result := '';

  while i <= ns.Len do
    begin
      Successed := False;
      j := umlMin(t2hkDict.HashList.MaxNameLen, ns.Len - i);
      while j > 1 do
        begin
          n2 := ns.Copy(i, j);
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
          n2 := t2HK_Table(ns[i], Successed);
          if Successed then
              Result.Append(n2)
          else
              Result.Append(ns[i]);
          inc(i);
        end;
    end;
end;

function t2s_Table(const s: TUPascalString; var Successed: Boolean): TUPascalString;
begin
  Result := t2sDict.GetDefaultValue(s, '');
  Successed := Result.Len > 0;
  if not Successed then
      Result := s;
end;

function T2S(const s: TUPascalString): TUPascalString;
var
  ns, n2, n3: TUPascalString;
  i, j: Integer;
  Successed: Boolean;
begin
  ns := s;
  i := 1;

  Result := '';

  while i <= ns.Len do
    begin
      Successed := False;
      j := umlMin(t2sDict.HashList.MaxNameLen, ns.Len - i);
      while j > 1 do
        begin
          n2 := ns.Copy(i, j);
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
          n2 := t2s_Table(ns[i], Successed);
          if Successed then
              Result.Append(n2)
          else
              Result.Append(ns[i]);
          inc(i);
        end;
    end;

  Result := GBKString(Result);
end;

function t2tw_Table(const s: TUPascalString; var Successed: Boolean): TUPascalString;
begin
  Result := t2twDict.GetDefaultValue(s, '');
  Successed := Result.Len > 0;
  if not Successed then
      Result := s;
end;

function S2TW(const s: TUPascalString): TUPascalString;
var
  ns, n2, n3: TUPascalString;
  i, j: Integer;
  Successed: Boolean;
begin
  ns := S2T(s);
  i := 1;

  Result := '';

  while i <= ns.Len do
    begin
      Successed := False;
      j := umlMin(t2twDict.HashList.MaxNameLen, ns.Len - i);
      while j > 1 do
        begin
          n2 := ns.Copy(i, j);
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
          n2 := t2tw_Table(ns[i], Successed);
          if Successed then
              Result.Append(n2)
          else
              Result.Append(ns[i]);
          inc(i);
        end;
    end;
end;

end.
