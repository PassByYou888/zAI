{ ****************************************************************************** }
{ * GBK Vector           written by QQ 600585@qq.com                           * }
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
unit GBKVec;

{$INCLUDE zDefine.inc}

interface

uses DoStatusIO, CoreClasses, PascalStrings, UPascalStrings, Variants,
  MemoryStream64, ListEngine, TextDataEngine, UnicodeMixedLib;

function WordPart(const s: TUPascalString; const Unidentified, Completed: TListPascalString): Integer; overload;
function WordPart(const s: TUPascalString): TPascalString; overload;
function WordPartN(const s: TUPascalString): TPascalString;
function WordPartD(const s: TUPascalString): TPascalString;

function WillVec(const s: TUPascalString): Integer;
function WordVec(const s: TUPascalString): Integer;

function BadEmotion(const s: TUPascalString): Integer;
function BadRep(const s: TUPascalString): Integer;
function GoodEmotion(const s: TUPascalString): Integer;
function GoodRep(const s: TUPascalString): Integer;

implementation

uses GBK, GBKMediaCenter;

function WordPart(const s: TUPascalString; const Unidentified, Completed: TListPascalString): Integer;
var
  ns, n2   : TUPascalString;
  i, j     : Integer;
  Successed: Boolean;
begin
  Completed.Clear;
  ns := GBKString(s);
  Result := 0;

  i := 1;
  while i <= ns.Len do
    begin
      Successed := False;
      j := umlMin(WordPartDict.MaxSectionNameLen, ns.Len - i);
      while j > 1 do
        begin
          n2 := ns.Copy(i, j);
          Successed := WordPartDict.Exists(n2);
          if Successed then
            begin
              Completed.Add(n2.Text, WordPartDict.VariantList[n2.Text]);
              inc(Result);
              inc(i, j);
              Break;
            end;
          dec(j);
        end;

      if not Successed then
        begin
          Successed := WordPartDict.Exists(ns[i]);
          if Successed then
            begin
              Completed.Add(ns[i], WordPartDict.VariantList[ns[i]]);
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

function WordPart(const s: TUPascalString): TPascalString;
var
  Unidentified: TListPascalString;
  Completed   : TListPascalString;
  i           : Integer;
begin
  Result := '';
  Unidentified := TListPascalString.Create;
  Completed := TListPascalString.Create;
  if WordPart(s, Unidentified, Completed) > 0 then
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

function WordPartN(const s: TUPascalString): TPascalString;
var
  Unidentified: TListPascalString;
  Completed   : TListPascalString;
  i           : Integer;
begin
  Result := '';
  Unidentified := TListPascalString.Create;
  Completed := TListPascalString.Create;
  if WordPart(s, Unidentified, Completed) > 0 then
    begin
      for i := 0 to Completed.Count - 1 do
        begin
          if Result.Len > 0 then
              Result.Append(' ');
          Result.Append(Completed[i].Text + '\' + VarToStr(THashVariantList(Completed.Objects[i]).GetDefaultValue('token', '')));
        end;
    end;
  DisposeObject([Unidentified, Completed]);
end;

function WordPartD(const s: TUPascalString): TPascalString;
var
  Unidentified: TListPascalString;
  Completed   : TListPascalString;
  i           : Integer;
begin
  Result := '';
  Unidentified := TListPascalString.Create;
  Completed := TListPascalString.Create;
  if WordPart(s, Unidentified, Completed) > 0 then
    begin
      for i := 0 to Completed.Count - 1 do
        begin
          if Result.Len > 0 then
              Result.Append(#13#10);
          Result.Append(Completed[i].Text + '(' + VarToStr(THashVariantList(Completed.Objects[i]).GetDefaultValue('desc', '')) + ')');
        end;
    end;
  DisposeObject([Unidentified, Completed]);
end;

function FullQuery_Table(const List: THashList; const s: TUPascalString): Integer; overload;
var
  ns, n2, n3: TUPascalString;
  i, j, L   : Integer;
  Successed : Boolean;
begin
  ns := GBKString(s);

  Result := 0;
  L := List.MaxNameLen;

  i := 1;
  while i <= ns.Len do
    begin
      Successed := False;
      j := umlMin(L, ns.Len - i);
      while j > 1 do
        begin
          n2 := ns.Copy(i, j);
          Successed := List.Exists(n2);
          if Successed then
            begin
              inc(Result);
              inc(i, j);
              Break;
            end;
          dec(j);
        end;

      if not Successed then
        begin
          Successed := List.Exists(ns[i]);
          if Successed then
              inc(Result);
          inc(i);
        end;
    end;
end;

function FullQuery_Table(const List: THashTextEngine; const s: TUPascalString): Integer; overload;
  function InternalQuery(const vl: THashVariantList; const ns: TUPascalString): Integer;
  var
    n2       : TUPascalString;
    i, j, L  : Integer;
    Successed: Boolean;
  begin
    Result := 0;
    L := vl.HashList.MaxNameLen;

    i := 1;
    while i <= ns.Len do
      begin
        Successed := False;
        j := umlMin(L, ns.Len - i);
        while j > 1 do
          begin
            n2 := ns.Copy(i, j);
            Successed := vl.Exists(n2);
            if Successed then
              begin
                inc(Result);
                inc(i, j);
                Break;
              end;
            dec(j);
          end;

        if not Successed then
          begin
            Successed := vl.Exists(ns[i]);
            if Successed then
                inc(Result);
            inc(i);
          end;
      end;
  end;

var
  ns  : TUPascalString;
  i, r: Integer;
  pl  : TListPascalString;
begin
  ns := GBKString(s);
  Result := 0;
  pl := TListPascalString.Create;
  List.GetSectionList(pl);
  for i := 0 to pl.Count - 1 do
    begin
      r := InternalQuery(List.VariantList[pl[i]], ns);
      inc(Result, umlStrToInt(pl[i]) * r);
    end;
  DisposeObject(pl);
end;

function WillVec(const s: TUPascalString): Integer;
begin
  Result := FullQuery_Table(WillVecDict, s);
end;

function WordVec(const s: TUPascalString): Integer;
begin
  Result := FullQuery_Table(WordVecDict, s);
end;

function BadEmotion(const s: TUPascalString): Integer;
begin
  Result := FullQuery_Table(BadEmotionDict, s);
end;

function BadRep(const s: TUPascalString): Integer;
begin
  Result := FullQuery_Table(BadRepDict, s);
end;

function GoodEmotion(const s: TUPascalString): Integer;
begin
  Result := FullQuery_Table(GoodEmotionDict, s);
end;

function GoodRep(const s: TUPascalString): Integer;
begin
  Result := FullQuery_Table(GoodRepDict, s);
end;

end. 
 
 
