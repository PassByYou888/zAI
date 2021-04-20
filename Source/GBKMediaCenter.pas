{ ****************************************************************************** }
{ * GBK media Data          writen by QQ 600585@qq.com                         * }
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
unit GBKMediaCenter;

{$INCLUDE zDefine.inc}

interface

uses Classes, DoStatusIO, CoreClasses, PascalStrings, UPascalStrings,
  MemoryStream64, ListEngine, TextDataEngine, UnicodeMixedLib,
  ObjectData, ObjectDataManager, ItemStream;

type
  TDictStyle = (dsChar, dsPY, dsS2T, dsT2HK, dsT2S, dsT2TW,
    dsWordPart,
    dsWillVec,
    dsWordVec,
    dsBadEmotion, dsBadRep, dsGoodEmotion, dsGoodRep,
    dsBigKey, dsBigWord
    );

const
  cDictName: array [TDictStyle] of string = (
    ('KEY-CHARACTER'),
    ('KEY-PY'),
    ('KEY-S2T'),
    ('KEY-T2HONGKONG'),
    ('KEY-T2S'),
    ('KEY-T2TW'),
    ('INI-WORDPART'),
    ('INI-WILL'),
    ('INI-VEC'),
    ('TEXT-BADEMOTION'),
    ('TEXT-DSBADREP'),
    ('TEXT-DSGOODEMOTION'),
    ('TEXT-DSGOODREP'),
    ('BIG-KEY'),
    ('BIG-TEXT'));

procedure WaitGBKMediaInit;
function GBKIsBusy: Boolean;
function LoadAndMergeDict(const ROOT_: TPascalString): NativeInt; overload;
function LoadAndMergeDict(const DBEng_: TObjectDataManager): NativeInt; overload;
procedure InitEmptyDBFile(const dbFile: TPascalString);
function InitEmptyDBStream: TMemoryStream64;

function MergeTo(data: THashStringList; DestDict: TDictStyle): Integer; overload;
function MergeTo(data: THashTextEngine; DestDict: TDictStyle): Integer; overload;
function MergeTo(data: THashList; DestDict: TDictStyle): Integer; overload;

function GBKStorePath(const ROOT_: TPascalString; const dict_: TDictStyle): TPascalString;
function GBKDBStorePath(const ROOT_: TPascalString; const dict_: TDictStyle): TPascalString;

function LoadDBPath(const dbEng: TObjectDataManager; const Path_, fileFilter: TPascalString; const mergeTo_: THashStringList): NativeInt; overload;
function LoadDBPath(const dbEng: TObjectDataManager; const Path_, fileFilter: TPascalString; const mergeTo_: THashList): NativeInt; overload;
function LoadDBPath(const dbEng: TObjectDataManager; const Path_, fileFilter: TPascalString; const mergeTo_: THashTextEngine): NativeInt; overload;
function LoadPath(const Path_, fileFilter: TPascalString; const mergeTo_: THashStringList): NativeInt; overload;
function LoadPath(const Path_, fileFilter: TPascalString; const mergeTo_: THashList): NativeInt; overload;
function LoadPath(const Path_, fileFilter: TPascalString; const mergeTo_: THashTextEngine): NativeInt; overload;

var
  // GBK base dict
  CharDict, PYDict, S2TDict, T2HKDict, T2SDict, T2TWDict: THashStringList;

  // word part
  WordPartDict: THashTextEngine;
  // will vec
  WillVecDict: THashTextEngine;
  // word vec
  WordVecDict: THashTextEngine;

  // bad emotion and rep dict
  BadEmotionDict, BadRepDict: THashList;
  // good emotion and rep dict
  GoodEmotionDict, GoodRepDict: THashList;

  // big key
  bigKeyDict: THashStringList;

  // big word
  bigWordDict: THashList;

implementation

uses Types;

{$RESOURCE GBK.RES}


var
  GBKMediaInited: TAtomBool;
  GBKProgressInfo: TAtomString;

const
  cAllDict = [dsChar, dsPY, dsS2T, dsT2HK, dsT2S, dsT2TW,
    dsWordPart,
    dsWillVec,
    dsWordVec,
    dsBadEmotion, dsBadRep, dsGoodEmotion, dsGoodRep,
    dsBigKey, dsBigWord];

function MergeTo(data: THashStringList; DestDict: TDictStyle): Integer;
begin
  Result := 0;
  case DestDict of
    dsChar: data.MergeTo(CharDict);
    dsPY: data.MergeTo(PYDict);
    dsS2T: data.MergeTo(S2TDict);
    dsT2HK: data.MergeTo(T2HKDict);
    dsT2S: data.MergeTo(T2SDict);
    dsT2TW: data.MergeTo(T2TWDict);
    dsBigKey: data.MergeTo(bigKeyDict);
  end;
end;

function MergeTo(data: THashTextEngine; DestDict: TDictStyle): Integer;
begin
  Result := 0;
  case DestDict of
    dsWordPart: WordPartDict.Merge(data);
    dsWillVec: WillVecDict.Merge(data);
    dsWordVec: WordVecDict.Merge(data);
  end;
end;

function MergeTo(data: THashList; DestDict: TDictStyle): Integer;
begin
  Result := 0;
  case DestDict of
    dsBadEmotion: data.MergeTo(BadEmotionDict);
    dsBadRep: data.MergeTo(BadRepDict);
    dsGoodEmotion: data.MergeTo(GoodEmotionDict);
    dsGoodRep: data.MergeTo(GoodRepDict);
    dsBigWord: data.MergeTo(bigWordDict);
  end;
end;

function GBKStorePath(const ROOT_: TPascalString; const dict_: TDictStyle): TPascalString;
begin
  Result := umlCombinePath(ROOT_, cDictName[dict_]);
end;

function GBKDBStorePath(const ROOT_: TPascalString; const dict_: TDictStyle): TPascalString;
begin
  Result := umlCombineUnixPath(ROOT_, cDictName[dict_]);
end;

function LoadDBPath(const dbEng: TObjectDataManager;
  const Path_, fileFilter: TPascalString; const mergeTo_: THashStringList): NativeInt;
var
  ori: NativeInt;
  rs: TItemRecursionSearch;
  itmHnd: TItemHandle;
  m64: TMemoryStream64;
begin
  Result := 0;
  if not dbEng.FieldExists(Path_) then
      exit;

  if dbEng.RecursionSearchFirst(Path_, '*', rs) then
    begin
      repeat
        if rs.ReturnHeader.ID = DB_Header_Item_ID then
          if umlMultipleMatch(fileFilter, rs.ReturnHeader.Name) then
            begin
              dbEng.ItemFastOpen(rs.ReturnHeader.CurrentHeader, itmHnd);
              m64 := TMemoryStream64.Create;
              dbEng.ItemReadToStream(itmHnd, m64);
              m64.Position := 0;

              // merge now
              ori := mergeTo_.Count;
              mergeTo_.LoadFromStream(m64);
              inc(Result, mergeTo_.Count - ori);

              disposeObject(m64);
              dbEng.ItemClose(itmHnd)
            end;
      until not dbEng.RecursionSearchNext(rs);
    end;
end;

function LoadDBPath(const dbEng: TObjectDataManager;
  const Path_, fileFilter: TPascalString; const mergeTo_: THashList): NativeInt;
var
  j: Integer;
  ori: NativeInt;
  rs: TItemRecursionSearch;
  itmHnd: TItemHandle;
  m64: TMemoryStream64;
  lst: TListPascalString;
begin
  Result := 0;
  if not dbEng.FieldExists(Path_) then
      exit;

  if dbEng.RecursionSearchFirst(Path_, '*', rs) then
    begin
      repeat
        if rs.ReturnHeader.ID = DB_Header_Item_ID then
          if umlMultipleMatch(fileFilter, rs.ReturnHeader.Name) then
            begin
              dbEng.ItemFastOpen(rs.ReturnHeader.CurrentHeader, itmHnd);
              m64 := TMemoryStream64.Create;
              dbEng.ItemReadToStream(itmHnd, m64);
              m64.Position := 0;

              // merge now
              lst := TListPascalString.Create;
              lst.LoadFromStream(m64);
              for j := 0 to lst.Count - 1 do
                  mergeTo_.Add(lst[j], nil, True);
              disposeObject(lst);
              inc(Result, mergeTo_.Count - ori);

              disposeObject(m64);
              dbEng.ItemClose(itmHnd)
            end;
      until not dbEng.RecursionSearchNext(rs);
    end;
end;

function LoadDBPath(const dbEng: TObjectDataManager;
  const Path_, fileFilter: TPascalString; const mergeTo_: THashTextEngine): NativeInt;
var
  ori: NativeInt;
  rs: TItemRecursionSearch;
  itmHnd: TItemHandle;
  m64: TMemoryStream64;
  te: THashTextEngine;
begin
  Result := 0;
  if not dbEng.FieldExists(Path_) then
      exit;

  if dbEng.RecursionSearchFirst(Path_, '*', rs) then
    begin
      repeat
        if rs.ReturnHeader.ID = DB_Header_Item_ID then
          if umlMultipleMatch(fileFilter, rs.ReturnHeader.Name) then
            begin
              dbEng.ItemFastOpen(rs.ReturnHeader.CurrentHeader, itmHnd);
              m64 := TMemoryStream64.Create;
              dbEng.ItemReadToStream(itmHnd, m64);
              m64.Position := 0;

              // merge now
              te := THashTextEngine.Create;
              te.LoadFromStream(m64);
              mergeTo_.Merge(te);
              disposeObject(te);
              inc(Result, mergeTo_.TotalCount - ori);

              disposeObject(m64);
              dbEng.ItemClose(itmHnd)
            end;
      until not dbEng.RecursionSearchNext(rs);
    end;
end;

function LoadPath(const Path_, fileFilter: TPascalString; const mergeTo_: THashStringList): NativeInt;
var
  fArry: U_StringArray;
  pArry: U_StringArray;
  i: Integer;
  ori: NativeInt;
begin
  Result := 0;
  if not umlDirectoryExists(Path_) then
      exit;

  fArry := umlGetFileListWithFullPath(Path_);
  for i := low(fArry) to high(fArry) do
    if umlMultipleMatch(fileFilter, umlGetFileName(fArry[i])) then
      begin
        ori := mergeTo_.Count;
        mergeTo_.LoadFromFile(fArry[i]);
        inc(Result, mergeTo_.Count - ori);
      end;
  SetLength(fArry, 0);

  pArry := umlGetDirListWithFullPath(Path_);
  for i := low(pArry) to high(pArry) do
      inc(Result, LoadPath(pArry[i], fileFilter, mergeTo_));
  SetLength(pArry, 0);
end;

function LoadPath(const Path_, fileFilter: TPascalString; const mergeTo_: THashList): NativeInt;
var
  fArry: U_StringArray;
  pArry: U_StringArray;
  i, j: Integer;
  lst: TListPascalString;
  ori: NativeInt;
begin
  Result := 0;
  if not umlDirectoryExists(Path_) then
      exit;

  fArry := umlGetFileListWithFullPath(Path_);
  for i := low(fArry) to high(fArry) do
    if umlMultipleMatch(fileFilter, umlGetFileName(fArry[i])) then
      begin
        ori := mergeTo_.Count;

        lst := TListPascalString.Create;
        lst.LoadFromFile(fArry[i]);
        for j := 0 to lst.Count - 1 do
            mergeTo_.Add(lst[j], nil, True);
        disposeObject(lst);

        inc(Result, mergeTo_.Count - ori);
      end;
  SetLength(fArry, 0);

  pArry := umlGetDirListWithFullPath(Path_);
  for i := low(pArry) to high(pArry) do
      inc(Result, LoadPath(pArry[i], fileFilter, mergeTo_));
  SetLength(pArry, 0);
end;

function LoadPath(const Path_, fileFilter: TPascalString; const mergeTo_: THashTextEngine): NativeInt;
var
  fArry: U_StringArray;
  pArry: U_StringArray;
  te: THashTextEngine;
  i: Integer;
  ori: NativeInt;
begin
  Result := 0;
  if not umlDirectoryExists(Path_) then
      exit;

  fArry := umlGetFileListWithFullPath(Path_);
  for i := low(fArry) to high(fArry) do
    if umlMultipleMatch(fileFilter, umlGetFileName(fArry[i])) then
      begin
        ori := mergeTo_.TotalCount;

        te := THashTextEngine.Create;
        te.LoadFromFile(fArry[i]);
        mergeTo_.Merge(te);
        disposeObject(te);

        inc(Result, mergeTo_.TotalCount - ori);
      end;
  SetLength(fArry, 0);

  pArry := umlGetDirListWithFullPath(Path_);
  for i := low(pArry) to high(pArry) do
      inc(Result, LoadPath(pArry[i], fileFilter, mergeTo_));
  SetLength(pArry, 0);
end;

function LoadAndMergeDict(const ROOT_: TPascalString): NativeInt;
var
  dict_: TDictStyle;
  r: NativeInt;
  ph: TPascalString;
begin
  WaitGBKMediaInit;
  GBKMediaInited.V := False;

  Result := 0;
  try
    for dict_ in cAllDict do
      begin
        ph := GBKStorePath(ROOT_, dict_);
        if not umlDirectoryExists(ph) then
            umlCreateDirectory(ph);

        r := 0;

        case dict_ of
          dsChar: r := LoadPath(ph, '*.txt', CharDict);
          dsPY: r := LoadPath(ph, '*.txt', PYDict);
          dsS2T: r := LoadPath(ph, '*.txt', S2TDict);
          dsT2HK: r := LoadPath(ph, '*.txt', T2HKDict);
          dsT2S: r := LoadPath(ph, '*.txt', T2SDict);
          dsT2TW: r := LoadPath(ph, '*.txt', T2TWDict);
          dsWordPart: r := LoadPath(ph, '*.ini;*.txt', WordPartDict);
          dsWillVec: r := LoadPath(ph, '*.ini;*.txt', WillVecDict);
          dsWordVec: r := LoadPath(ph, '*.ini;*.txt', WordVecDict);
          dsBadEmotion: r := LoadPath(ph, '*.txt', BadEmotionDict);
          dsBadRep: r := LoadPath(ph, '*.txt', BadRepDict);
          dsGoodEmotion: r := LoadPath(ph, '*.txt', GoodEmotionDict);
          dsGoodRep: r := LoadPath(ph, '*.txt', GoodRepDict);
          dsBigKey: r := LoadPath(ph, '*.txt', bigKeyDict);
          dsBigWord: r := LoadPath(ph, '*.txt', bigWordDict);
        end;

        DoStatus('%s loaded %d ...', [cDictName[dict_], r]);
        inc(Result, r);
      end;
  finally
      GBKMediaInited.V := True;
  end;
end;

function LoadAndMergeDict(const DBEng_: TObjectDataManager): NativeInt;
var
  dict_: TDictStyle;
  r: NativeInt;
  ph: TPascalString;
begin
  WaitGBKMediaInit;
  GBKMediaInited.V := False;

  Result := 0;
  try
    for dict_ in cAllDict do
      begin
        ph := GBKDBStorePath('/', dict_);
        if not DBEng_.FieldExists(ph) then
            DBEng_.CreateField(ph, '');

        r := 0;

        case dict_ of
          dsChar: r := LoadDBPath(DBEng_, ph, '*.txt', CharDict);
          dsPY: r := LoadDBPath(DBEng_, ph, '*.txt', PYDict);
          dsS2T: r := LoadDBPath(DBEng_, ph, '*.txt', S2TDict);
          dsT2HK: r := LoadDBPath(DBEng_, ph, '*.txt', T2HKDict);
          dsT2S: r := LoadDBPath(DBEng_, ph, '*.txt', T2SDict);
          dsT2TW: r := LoadDBPath(DBEng_, ph, '*.txt', T2TWDict);
          dsWordPart: r := LoadDBPath(DBEng_, ph, '*.ini;*.txt', WordPartDict);
          dsWillVec: r := LoadDBPath(DBEng_, ph, '*.ini;*.txt', WillVecDict);
          dsWordVec: r := LoadDBPath(DBEng_, ph, '*.ini;*.txt', WordVecDict);
          dsBadEmotion: r := LoadDBPath(DBEng_, ph, '*.txt', BadEmotionDict);
          dsBadRep: r := LoadDBPath(DBEng_, ph, '*.txt', BadRepDict);
          dsGoodEmotion: r := LoadDBPath(DBEng_, ph, '*.txt', GoodEmotionDict);
          dsGoodRep: r := LoadDBPath(DBEng_, ph, '*.txt', GoodRepDict);
          dsBigKey: r := LoadDBPath(DBEng_, ph, '*.txt', bigKeyDict);
          dsBigWord: r := LoadDBPath(DBEng_, ph, '*.txt', bigWordDict);
        end;

        DoStatus('%s loaded %d ...', [cDictName[dict_], r]);
        inc(Result, r);
      end;
  finally
      GBKMediaInited.V := True;
  end;
end;

procedure InitEmptyDBFile(const dbFile: TPascalString);
var
  m64: TMemoryStream64;
begin
  with InitEmptyDBStream() do
    begin
      SaveToFile(dbFile);
      Free;
    end;
end;

function InitEmptyDBStream: TMemoryStream64;
var
  dbEng: TObjectDataManager;
  dict_: TDictStyle;
begin
  Result := TMemoryStream64.CustomCreate($FFFF);
  dbEng := TObjectDataManagerOfCache.CreateAsStream($FF, Result, '', DBmarshal.ID, False, True, False);
  for dict_ in cAllDict do
      dbEng.CreateField(GBKDBStorePath('/', dict_), '');
  disposeObject(dbEng);
end;

function GetGBKTextEngineDict(dbEng: TObjectDataManagerOfCache; fn: U_String; hashSiz: NativeInt): THashTextEngine;
var
  stream: TMemoryStream64;
begin
{$IFDEF initializationStatus}
  DoStatusNoLn('Load INI Dict %s', [fn.Text]);
{$ENDIF initializationStatus}
  stream := TMemoryStream64.Create;
  dbEng.ItemReadToStream('/', fn, stream);
  stream.Position := 0;

  Result := THashTextEngine.Create(hashSiz);
  Result.LoadFromStream(stream);
  disposeObject(stream);

{$IFDEF initializationStatus}
  DoStatusNoLn(' done.');
  DoStatusNoLn;
{$ENDIF initializationStatus}
end;

function GetGBKHashStringDict(dbEng: TObjectDataManagerOfCache; fn: U_String; hashSiz: NativeInt): THashStringList;
var
  stream: TMemoryStream64;
begin
{$IFDEF initializationStatus}
  DoStatusNoLn('Load Hash Dict %s', [fn.Text]);
{$ENDIF initializationStatus}
  stream := TMemoryStream64.Create;
  dbEng.ItemReadToStream('/', fn, stream);
  stream.Position := 0;
  Result := THashStringList.CustomCreate(hashSiz);
  Result.LoadFromStream(stream);
  disposeObject(stream);
{$IFDEF initializationStatus}
  DoStatusNoLn(' done.');
  DoStatusNoLn;
{$ENDIF initializationStatus}
end;

function GetGBKHashDict(dbEng: TObjectDataManagerOfCache; fn: U_String; hashSiz: NativeInt): THashList;
var
  stream: TMemoryStream64;
  lst: TListPascalString;
  i: Integer;
begin
{$IFDEF initializationStatus}
  DoStatusNoLn('Load Big Dict %s', [fn.Text]);
{$ENDIF initializationStatus}
  stream := TMemoryStream64.Create;
  dbEng.ItemReadToStream('/', fn, stream);
  stream.Position := 0;

  Result := THashList.CustomCreate(hashSiz);

  lst := TListPascalString.Create;
  lst.LoadFromStream(stream);
  disposeObject(stream);
  for i := 0 to lst.Count - 1 do
      Result.Add(lst[i], nil, True);
  disposeObject(lst);
{$IFDEF initializationStatus}
  DoStatusNoLn(' done.');
  DoStatusNoLn;
{$ENDIF initializationStatus}
end;

procedure InitGBKMediaThread(th: TCompute);
var
  dbEng: TObjectDataManagerOfCache;
  rs: TCoreClassResourceStream;
  tmpStream_: TMemoryStream64;
begin
  rs := TCoreClassResourceStream.Create(HInstance, 'GBK', RT_RCDATA);
  tmpStream_ := TMemoryStream64.Create;
  DecompressStream(rs, tmpStream_);
  disposeObject(rs);

  dbEng := TObjectDataManagerOfCache.CreateAsStream(tmpStream_, '', DBmarshal.ID, True, False, True);

  // base gbk dict
  CharDict := GetGBKHashStringDict(dbEng, 'CharDict.BuildIn', 20000);
  PYDict := GetGBKHashStringDict(dbEng, 'PYDict.BuildIn', 20000);
  S2TDict := GetGBKHashStringDict(dbEng, 'S2T.BuildIn', 20000);
  T2HKDict := GetGBKHashStringDict(dbEng, 'T2HK.BuildIn', 20000);
  T2SDict := GetGBKHashStringDict(dbEng, 'T2S.BuildIn', 20000);
  T2TWDict := GetGBKHashStringDict(dbEng, 'T2TW.BuildIn', 20000);

  // word part dict
  WordPartDict := GetGBKTextEngineDict(dbEng, 'WordPart.BuildIn', 50000);

  // will vec dict
  WillVecDict := GetGBKTextEngineDict(dbEng, 'WillVec.BuildIn', 5000);
  WordVecDict := GetGBKTextEngineDict(dbEng, 'WordVec.BuildIn', 5000);

  // emotion and rep dict
  BadEmotionDict := GetGBKHashDict(dbEng, 'BadEmotion.BuildIn', 20000);
  BadRepDict := GetGBKHashDict(dbEng, 'BadRep.BuildIn', 20000);
  GoodEmotionDict := GetGBKHashDict(dbEng, 'GoodEmotion.BuildIn', 20000);
  GoodRepDict := GetGBKHashDict(dbEng, 'GoodRep.BuildIn', 20000);

  // big key
  bigKeyDict := GetGBKHashStringDict(dbEng, 'MiniKey.BuildIn', 200 * 10000);
  // big word
  bigWordDict := GetGBKHashDict(dbEng, 'MiniDict.BuildIn', 200 * 10000);

  disposeObject(dbEng);

  GBKMediaInited.V := True;
end;

procedure FreeGBKMedia;
begin
  WaitGBKMediaInit;
  disposeObject([WordPartDict]);
  disposeObject([WillVecDict, WordVecDict]);
  disposeObject([BadEmotionDict, BadRepDict, GoodEmotionDict, GoodRepDict]);
  disposeObject([CharDict, PYDict, S2TDict, T2HKDict, T2SDict, T2TWDict]);
  disposeObject(bigKeyDict);
  disposeObject(bigWordDict);
end;

procedure WaitGBKMediaInit;
begin
  while not GBKMediaInited.V do
      CoreClasses.CheckThreadSynchronize(10);
end;

function GBKIsBusy: Boolean;
begin
  Result := not GBKMediaInited.V;
end;

initialization

GBKMediaInited := TAtomBool.Create(False);
GBKProgressInfo := TAtomString.Create('');
TCompute.RunC({$IFDEF FPC}@{$ENDIF FPC}InitGBKMediaThread);

finalization

FreeGBKMedia;
DisposeObjectAndNil(GBKMediaInited);
DisposeObjectAndNil(GBKProgressInfo);

end.
