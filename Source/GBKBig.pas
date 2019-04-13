{ ****************************************************************************** }
{ * GBK with Big text data           written by QQ 600585@qq.com               * }
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
unit GBKBig;

{$INCLUDE zDefine.inc}

interface

uses DoStatusIO, CoreClasses, PascalStrings, MemoryStream64, ListEngine, UnicodeMixedLib,
  UPascalStrings;

procedure BigKeyAnalysis(const Analysis: THashVariantList);
function BigKey(const s: TUPascalString; const MatchSingleWord: Boolean; const Unidentified, Completed: TListPascalString): Integer;
function BigKeyValue(const s: TUPascalString; const MatchSingleWord: Boolean; const Analysis: THashVariantList): Integer;
function BigKeyWord(const s: TUPascalString; const MatchSingleWord: Boolean): TPascalString;

function BigWord(const s: TUPascalString; const MatchSingleWord: Boolean; const Unidentified, Completed: TListPascalString): Integer; overload;
function BigWord(const s: TUPascalString; const MatchSingleWord: Boolean): TPascalString; overload;

implementation

uses GBKMediaCenter, GBK, GBKVec;

type
  TBigKeyAnalysis = class
    output: THashVariantList;
    procedure doProgress(Sender: THashStringList; Name: PSystemString; const v: SystemString);
  end;

procedure TBigKeyAnalysis.doProgress(Sender: THashStringList; Name: PSystemString; const v: SystemString);
begin
  umlSeparatorText(v, output, ',;'#9);
end;

procedure BigKeyAnalysis(const Analysis: THashVariantList);
var
  tmp: TBigKeyAnalysis;
begin
  tmp := TBigKeyAnalysis.Create;
  tmp.output := Analysis;
  bigKeyDict.ProgressM({$IFDEF FPC}@{$ENDIF FPC}tmp.doProgress);
  DisposeObject(tmp);
end;

function BigKey(const s: TUPascalString; const MatchSingleWord: Boolean; const Unidentified, Completed: TListPascalString): Integer;
var
  ns, n2: TUPascalString;
  i, j: Integer;
  Successed: Boolean;
begin
  ns := GBKString(s);
  Result := 0;

  i := 1;
  while i <= ns.Len do
    begin
      Successed := False;
      j := umlMin(bigKeyDict.HashList.MaxNameLen, ns.Len - i);
      while j > 1 do
        begin
          n2 := ns.Copy(i, j);
          Successed := bigKeyDict.Exists(n2);
          if Successed then
            begin
              Completed.Add(n2.Text);
              inc(Result);
              inc(i, j);
              Break;
            end;
          dec(j);
        end;

      if not Successed then
        begin
          Successed := MatchSingleWord and bigKeyDict.Exists(ns[i]);
          if Successed then
            begin
              Completed.Add(ns[i]);
              inc(Result);
            end
          else
            begin
              Unidentified.Add(ns[i]);
            end;
          inc(i);
        end;
    end;
end;

function BigKeyValue(const s: TUPascalString; const MatchSingleWord: Boolean; const Analysis: THashVariantList): Integer;
var
  Unidentified: TListPascalString;
  Completed: TListPascalString;
  i: Integer;
begin
  Analysis.Clear;
  Unidentified := TListPascalString.Create;
  Completed := TListPascalString.Create;
  Result := BigKey(s, MatchSingleWord, Unidentified, Completed);
  if Result > 0 then
    for i := 0 to Completed.Count - 1 do
        umlSeparatorText(bigKeyDict.GetDefaultValue(Completed[i], ''), Analysis, ',;'#9);
  DisposeObject([Unidentified, Completed]);
end;

function BigKeyWord(const s: TUPascalString; const MatchSingleWord: Boolean): TPascalString;
var
  Unidentified: TListPascalString;
  Completed: TListPascalString;
  i: Integer;
begin
  Result := '';
  Unidentified := TListPascalString.Create;
  Completed := TListPascalString.Create;
  if BigKey(s, MatchSingleWord, Unidentified, Completed) > 0 then
    begin
      for i := 0 to Completed.Count - 1 do
        begin
          if Result.Len > 0 then
              Result.Append(',');
          Result.Append(Completed[i]);
        end;
    end;
  DisposeObject([Unidentified, Completed]);
end;

function BigWord(const s: TUPascalString; const MatchSingleWord: Boolean; const Unidentified, Completed: TListPascalString): Integer;
var
  ns, n2: TUPascalString;
  i, j: Integer;
  Successed: Boolean;
begin
  ns := GBKString(s);
  Result := 0;

  i := 1;
  while i <= ns.Len do
    begin
      Successed := False;
      j := umlMin(bigWordDict.MaxNameLen, ns.Len - i);
      while j > 1 do
        begin
          n2 := ns.Copy(i, j);
          Successed := bigWordDict.Exists(n2);
          if Successed then
            begin
              Completed.Add(n2.Text);
              inc(Result);
              inc(i, j);
              Break;
            end;
          dec(j);
        end;

      if not Successed then
        begin
          Successed := MatchSingleWord and bigWordDict.Exists(ns[i]);
          if Successed then
            begin
              Completed.Add(ns[i]);
              inc(Result);
            end
          else
            begin
              Unidentified.Add(ns[i]);
            end;
          inc(i);
        end;
    end;
end;

function BigWord(const s: TUPascalString; const MatchSingleWord: Boolean): TPascalString;
var
  Unidentified: TListPascalString;
  Completed: TListPascalString;
  i: Integer;
begin
  Result := '';
  Unidentified := TListPascalString.Create;
  Completed := TListPascalString.Create;
  if BigWord(s, MatchSingleWord, Unidentified, Completed) > 0 then
    begin
      for i := 0 to Completed.Count - 1 do
        begin
          if Result.Len > 0 then
              Result.Append(',');
          Result.Append(Completed[i]);
        end;
    end;
  DisposeObject([Unidentified, Completed]);
end;

initialization

end.
