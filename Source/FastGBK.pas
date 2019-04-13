{ ****************************************************************************** }
{ * Core class library  written by QQ 600585@qq.com                            * }
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
unit FastGBK;

{$INCLUDE zDefine.inc}

interface


uses DoStatusIO, CoreClasses, PascalStrings, MemoryStream64, ListEngine, UnicodeMixedLib,
  UPascalStrings;

{ Quick query characters using the GBK encoding table }
function FastGBKChar(const c: USystemChar): Boolean; overload;
{ Quick query string using the GBK encoding table }
function FastGBKString(const s: TUPascalString): Boolean; overload;
{ Fast translation of Pinyin with GBK encoding table (not supporting phonetic) }
function FastPY(const s: TUPascalString; const multiPy: Boolean): TUPascalString;

{ Using the GBK coding table to sort out the phonetic alphabet quickly }
procedure FastPYSort(const inverse: Boolean; const inBuff: PArrayPascalString; var OutBuff: TArrayPascalStringPtr);

{ custom sort }
type
  TFastCompareFuncCall   = function(const v1, v2: PPascalString): ShortInt;
  TFastCompareFuncMethod = function(const v1, v2: PPascalString): ShortInt of object;

{$IFNDEF FPC} TFastCompareFuncProc = reference to function(const v1, v2: PPascalString): ShortInt; {$ENDIF FPC}

procedure FastCustomSort(const inBuff: PArrayPascalString; var OutBuff: TArrayPascalStringPtr; const OnCompare: TFastCompareFuncCall); overload;
procedure FastCustomSort(const inBuff: PArrayPascalString; var OutBuff: TArrayPascalStringPtr; const OnCompare: TFastCompareFuncMethod); overload;
{$IFNDEF FPC} procedure FastCustomSort(const inBuff: PArrayPascalString; var OutBuff: TArrayPascalStringPtr; const OnCompare: TFastCompareFuncProc); overload; {$ENDIF FPC}


var
  GBKCache: array [$FF .. $FFFF] of TUPascalString;

implementation

uses GBKMediaCenter, SysUtils;

function FastGBKChar(const c: USystemChar): Boolean;
var
  ID: Word;
  n: TUPascalString;
begin
  ID := Ord(c);
  if (ID >= $FF) and (ID <= $FFFF) then
      n := GBKCache[ID]
  else
      n := '';
  Result := n.Len > 0;
end;

function FastGBKString(const s: TUPascalString): Boolean;
var
  i: Integer;
begin
  Result := False;
  if s.Len = 0 then
      Exit;
  for i := 1 to s.Len do
    if not FastGBKChar(s[i]) then
        Exit;
  Result := True;
end;

function FastPY(const s: TUPascalString; const multiPy: Boolean): TUPascalString;
var
  n: TUPascalString;
  c: USystemChar;
  LastGBK: Boolean;
  ID: Word;
begin
  Result := '';
  LastGBK := False;
  for c in s.buff do
    begin
      ID := Ord(c);
      if (ID >= $FF) and (ID <= $FFFF) then
          n := GBKCache[ID]
      else
          n := '';

      if n.Len > 0 then
        begin
          if n.Exists(',') then
            begin
              if multiPy then
                  n.Text := '(' + n.Text + ')'
              else
                  n := umlGetFirstStr(n.Text, ',');
            end;

          if (Result.Len > 0) then
            begin
              if LastGBK then
                  Result.Append(#32);
            end;

          LastGBK := True;
        end
      else
        begin
          n := c;

          if LastGBK then
            begin
              Result.Append(#32);
              LastGBK := False;
            end;
        end;
      Result.Append(n);
    end;
  n := '';
end;

procedure FastPYSort(const inverse: Boolean; const inBuff: PArrayPascalString; var OutBuff: TArrayPascalStringPtr);

  function cv(const a, b: Integer): Integer;
  begin
    if a = b then
        Result := 0
    else if a < b then
        Result := -1
    else
        Result := 1;
  end;

  function WasWide(const t: PPascalString): Byte;
  var
    c: USystemChar;
  begin
    for c in t^.buff do
      if FastGBKChar(c) then
          Exit(1);
    Result := 0;
  end;

  function CompText(t1, t2: PPascalString): Integer;
  var
    t3: PPascalString;
  begin
    if inverse then
      begin
        t3 := t1;
        t1 := t2;
        t2 := t3;
      end;

    Result := cv(WasWide(t1), WasWide(t2));
    if Result = 0 then
      begin
        Result := cv(t1^.Len, t2^.Len);
        if (Result = 0) and (t1^.Len > 0) then
            Result := CompareText(FastPY(t1^, False).Text, FastPY(t2^, False).Text);
      end;
  end;

  procedure QuickSortList(var SortList: TArrayPascalStringPtr; L, r: Integer);
  var
    i, j: Integer;
    p, t: PPascalString;
  begin
    repeat
      i := L;
      j := r;
      p := SortList[(L + r) shr 1];
      repeat
        while CompText(SortList[i], p) < 0 do
            inc(i);
        while CompText(SortList[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortList[i];
                SortList[i] := SortList[j];
                SortList[j] := t;
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          QuickSortList(SortList, L, j);
      L := i;
    until i >= r;
  end;

var
  i: Integer;
begin
  SetLength(OutBuff, length(inBuff^));
  for i := low(inBuff^) to high(inBuff^) do
      OutBuff[i] := @inBuff^[i];

  if length(OutBuff) > 1 then
      QuickSortList(OutBuff, low(OutBuff), high(OutBuff));
end;

procedure FastCustomSort(const inBuff: PArrayPascalString; var OutBuff: TArrayPascalStringPtr; const OnCompare: TFastCompareFuncCall);
  procedure QuickSortList(var SortList: TArrayPascalStringPtr; L, r: Integer);
  var
    i, j: Integer;
    p, t: PPascalString;
  begin
    repeat
      i := L;
      j := r;
      p := SortList[(L + r) shr 1];
      repeat
        while OnCompare(SortList[i], p) < 0 do
            inc(i);
        while OnCompare(SortList[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortList[i];
                SortList[i] := SortList[j];
                SortList[j] := t;
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          QuickSortList(SortList, L, j);
      L := i;
    until i >= r;
  end;

var
  i: Integer;
begin
  SetLength(OutBuff, length(inBuff^));
  for i := low(inBuff^) to high(inBuff^) do
      OutBuff[i] := @inBuff^[i];

  if length(OutBuff) > 1 then
      QuickSortList(OutBuff, low(OutBuff), high(OutBuff));
end;

procedure FastCustomSort(const inBuff: PArrayPascalString; var OutBuff: TArrayPascalStringPtr; const OnCompare: TFastCompareFuncMethod);
  procedure QuickSortList(var SortList: TArrayPascalStringPtr; L, r: Integer);
  var
    i, j: Integer;
    p, t: PPascalString;
  begin
    repeat
      i := L;
      j := r;
      p := SortList[(L + r) shr 1];
      repeat
        while OnCompare(SortList[i], p) < 0 do
            inc(i);
        while OnCompare(SortList[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortList[i];
                SortList[i] := SortList[j];
                SortList[j] := t;
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          QuickSortList(SortList, L, j);
      L := i;
    until i >= r;
  end;

var
  i: Integer;
begin
  SetLength(OutBuff, length(inBuff^));
  for i := low(inBuff^) to high(inBuff^) do
      OutBuff[i] := @inBuff^[i];

  if length(OutBuff) > 1 then
      QuickSortList(OutBuff, low(OutBuff), high(OutBuff));
end;

{$IFNDEF FPC}


procedure FastCustomSort(const inBuff: PArrayPascalString; var OutBuff: TArrayPascalStringPtr; const OnCompare: TFastCompareFuncProc);
  procedure QuickSortList(var SortList: TArrayPascalStringPtr; L, r: Integer);
  var
    i, j: Integer;
    p, t: PPascalString;
  begin
    repeat
      i := L;
      j := r;
      p := SortList[(L + r) shr 1];
      repeat
        while OnCompare(SortList[i], p) < 0 do
            inc(i);
        while OnCompare(SortList[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortList[i];
                SortList[i] := SortList[j];
                SortList[j] := t;
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          QuickSortList(SortList, L, j);
      L := i;
    until i >= r;
  end;

var
  i: Integer;
begin
  SetLength(OutBuff, length(inBuff^));
  for i := low(inBuff^) to high(inBuff^) do
      OutBuff[i] := @inBuff^[i];

  if length(OutBuff) > 1 then
      QuickSortList(OutBuff, low(OutBuff), high(OutBuff));
end;
{$ENDIF FPC}


procedure InitFastGBK;
// gbk with unpack format(unicode)
// char=py1,py2,py3
var
  output: TMemoryStream64;
  lst: TListPascalString;
  n: TUPascalString;
  i: Integer;
begin
  output := TMemoryStream64.Create;
  DecompressStream(@C_FastGBKPackageBuffer[0], 71628, output);
  output.Position := 0;
  lst := TListPascalString.Create;
  lst.LoadFromStream(output);

  for i := low(GBKCache) to high(GBKCache) do
      GBKCache[i] := '';

  for i := 0 to lst.Count - 1 do
    begin
      n := lst[i];
      GBKCache[Ord(TUPascalString(umlGetFirstStr(n.Text, '=')).First)] := umlDeleteFirstStr(n.Text, '=');
    end;
  DisposeObject([lst, output]);
end;

procedure FreeFastGBK;
var
  i: Integer;
begin
  for i := low(GBKCache) to high(GBKCache) do
      GBKCache[i] := '';
end;

initialization

InitFastGBK;

finalization

FreeFastGBK;

end.
