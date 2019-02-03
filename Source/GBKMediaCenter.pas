{ ****************************************************************************** }
{ * GBK media Data support, writen by QQ 600585@qq.com                         * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit GBKMediaCenter;

{$INCLUDE zDefine.inc}

interface

uses DoStatusIO, CoreClasses, PascalStrings, UPascalStrings,
  MemoryStream64, ListEngine, TextDataEngine, UnicodeMixedLib;

{$REGION 'GBKMediaCenterDecl'}


var
  // gbk base dict
  CharDict, PYDict, s2tDict, t2hkDict, t2sDict, t2twDict: THashStringList;

  // word part
  WordPartDict: THashTextEngine;
  // will vec
  WillVecDict: THashTextEngine;
  // word vec
  WordVecDict: THashTextEngine;

  // emotion and rep dict
  BadEmotionDict, BadRepDict, GoodEmotionDict, GoodRepDict: THashList;

  // big key
  bigKeyDict: THashStringList;

  // big word
  bigWordDict: THashList;

{$INCLUDE GBK_Dict.inc}
{$INCLUDE GBKVec_Dict.inc}
{$INCLUDE GBKWordPart_Dict.inc}
{$INCLUDE GBKBig_MiniDict.inc}
{$INCLUDE FastGBK_Dict.inc}

{$ENDREGION 'GBKMediaCenterDecl'}

function LoadAndMergeDict(const ROOT: TPascalString): NativeInt;

implementation


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
    ('BIG-TEXT')
    );

function GBKStorePath(const ROOT: TPascalString; const DS: TDictStyle): TPascalString;
begin
  Result := umlCombinePath(ROOT, cDictName[DS]);
end;

function LoadPath(const Path, fileFilter: TPascalString; const mergeTo: THashStringList): NativeInt; overload;
var
  fArry: U_StringArray;
  pArry: U_StringArray;
  i: Integer;
  ori: NativeInt;
begin
  Result := 0;
  if not umlDirectoryExists(Path) then
      Exit;

  fArry := umlGetFileListWithFullPath(Path);
  for i := low(fArry) to high(fArry) do
    if umlMultipleMatch(fileFilter, umlGetFileName(fArry[i])) then
      begin
        ori := mergeTo.Count;
        mergeTo.LoadFromFile(fArry[i]);
        inc(Result, mergeTo.Count - ori);
      end;
  SetLength(fArry, 0);

  pArry := umlGetDirListWithFullPath(Path);
  for i := low(pArry) to high(pArry) do
      inc(Result, LoadPath(pArry[i], fileFilter, mergeTo));
  SetLength(pArry, 0);
end;

function LoadPath(const Path, fileFilter: TPascalString; const mergeTo: THashList): NativeInt; overload;
var
  fArry: U_StringArray;
  pArry: U_StringArray;
  i, j: Integer;
  lst: TListPascalString;
  ori: NativeInt;
begin
  Result := 0;
  if not umlDirectoryExists(Path) then
      Exit;

  fArry := umlGetFileListWithFullPath(Path);
  for i := low(fArry) to high(fArry) do
    if umlMultipleMatch(fileFilter, umlGetFileName(fArry[i])) then
      begin
        ori := mergeTo.Count;

        lst := TListPascalString.Create;
        lst.LoadFromFile(fArry[i]);
        for j := 0 to lst.Count - 1 do
            mergeTo.Add(lst[j], nil, True);
        DisposeObject(lst);

        inc(Result, mergeTo.Count - ori);
      end;
  SetLength(fArry, 0);

  pArry := umlGetDirListWithFullPath(Path);
  for i := low(pArry) to high(pArry) do
      inc(Result, LoadPath(pArry[i], fileFilter, mergeTo));
  SetLength(pArry, 0);
end;

function LoadPath(const Path, fileFilter: TPascalString; const mergeTo: THashTextEngine): NativeInt; overload;
var
  fArry: U_StringArray;
  pArry: U_StringArray;
  te: THashTextEngine;
  i: Integer;
  ori: NativeInt;
begin
  Result := 0;
  if not umlDirectoryExists(Path) then
      Exit;

  fArry := umlGetFileListWithFullPath(Path);
  for i := low(fArry) to high(fArry) do
    if umlMultipleMatch(fileFilter, umlGetFileName(fArry[i])) then
      begin
        ori := mergeTo.TotalCount;

        te := THashTextEngine.Create;
        te.LoadFromFile(fArry[i]);
        mergeTo.Merge(te);
        DisposeObject(te);

        inc(Result, mergeTo.TotalCount - ori);
      end;
  SetLength(fArry, 0);

  pArry := umlGetDirListWithFullPath(Path);
  for i := low(pArry) to high(pArry) do
      inc(Result, LoadPath(pArry[i], fileFilter, mergeTo));
  SetLength(pArry, 0);
end;

function LoadAndMergeDict(const ROOT: TPascalString): NativeInt;
const
  cAllDict = [dsChar, dsPY, dsS2T, dsT2HK, dsT2S, dsT2TW,
    dsWordPart,
    dsWillVec,
    dsWordVec,
    dsBadEmotion, dsBadRep, dsGoodEmotion, dsGoodRep,
    dsBigKey, dsBigWord];

var
  DS: TDictStyle;
  r: NativeInt;
  ph: TPascalString;
begin
  Result := 0;
  for DS in cAllDict do
    begin
      ph := GBKStorePath(ROOT, DS);
      if not umlDirectoryExists(ph) then
          umlCreateDirectory(ph);

      r := 0;

      case DS of
        dsChar: r := LoadPath(ph, '*.txt', CharDict);
        dsPY: r := LoadPath(ph, '*.txt', PYDict);
        dsS2T: r := LoadPath(ph, '*.txt', s2tDict);
        dsT2HK: r := LoadPath(ph, '*.txt', t2hkDict);
        dsT2S: r := LoadPath(ph, '*.txt', t2sDict);
        dsT2TW: r := LoadPath(ph, '*.txt', t2twDict);
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

      DoStatus('%s loaded %d ...', [cDictName[DS], r]);
      inc(Result, r);
    end;
end;

function GetGBKTextEngineDict(Data: Pointer; siz, hashSiz: NativeInt): THashTextEngine;
var
  output: TMemoryStream64;
begin
  output := TMemoryStream64.Create;
  DecompressStream(Data, siz, output);
  output.Position := 0;
  Result := THashTextEngine.Create(hashSiz);
  Result.LoadFromStream(output);
  DisposeObject(output);
end;

function GetGBKHashStringDict(Data: Pointer; siz, hashSiz: NativeInt): THashStringList;
var
  output: TMemoryStream64;
begin
  output := TMemoryStream64.Create;
  DecompressStream(Data, siz, output);
  output.Position := 0;
  Result := THashStringList.CustomCreate(hashSiz);
  Result.LoadFromStream(output);
  DisposeObject(output);
end;

function GetGBKHashDict(Data: Pointer; siz, hashSiz: NativeInt): THashList;
var
  output: TMemoryStream64;
  lst: TListPascalString;
  i: Integer;
begin
  output := TMemoryStream64.Create;
  DecompressStream(Data, siz, output);
  output.Position := 0;
  Result := THashList.CustomCreate(hashSiz);

  lst := TListPascalString.Create;
  lst.LoadFromStream(output);
  DisposeObject(output);
  for i := 0 to lst.Count - 1 do
      Result.Add(lst[i], nil, True);
  DisposeObject(lst);
end;

procedure InitGBKMedia;
begin
  // base gbk dict
  CharDict := GetGBKHashStringDict(@C_CharDictPackageBuffer[0], SizeOf(T_CharDict_PackageBuffer), 20000);
  PYDict := GetGBKHashStringDict(@C_PYDictPackageBuffer[0], SizeOf(T_PYDict_PackageBuffer), 20000);
  s2tDict := GetGBKHashStringDict(@C_s2tPackageBuffer[0], SizeOf(T_s2t_PackageBuffer), 20000);
  t2hkDict := GetGBKHashStringDict(@C_t2hkPackageBuffer[0], SizeOf(T_t2hk_PackageBuffer), 20000);
  t2sDict := GetGBKHashStringDict(@C_t2sPackageBuffer[0], SizeOf(T_t2s_PackageBuffer), 20000);
  t2twDict := GetGBKHashStringDict(@C_t2twPackageBuffer[0], SizeOf(T_t2tw_PackageBuffer), 20000);

  // word part dict
  WordPartDict := GetGBKTextEngineDict(@C_WordPartPackageBuffer[0], SizeOf(T_WordPart_PackageBuffer), 50000);

  // will vec dict
  WillVecDict := GetGBKTextEngineDict(@C_willVecDictPackageBuffer[0], SizeOf(T_willVecDict_PackageBuffer), 5000);

  // word vec dict
  WordVecDict := GetGBKTextEngineDict(@C_WordVecDictPackageBuffer[0], SizeOf(T_WordVecDict_PackageBuffer), 5000);

  // emotion and rep dict
  BadEmotionDict := GetGBKHashDict(@C_BadEmotionDictPackageBuffer[0], SizeOf(T_BadEmotionDict_PackageBuffer), 20000);
  BadRepDict := GetGBKHashDict(@C_BadRepDictPackageBuffer[0], SizeOf(T_BadRepDict_PackageBuffer), 20000);
  GoodEmotionDict := GetGBKHashDict(@C_GoodEmotionDictPackageBuffer[0], SizeOf(T_GoodEmotionDict_PackageBuffer), 20000);
  GoodRepDict := GetGBKHashDict(@C_GoodRepDictPackageBuffer[0], SizeOf(T_GoodRepDict_PackageBuffer), 20000);

  // big key
  bigKeyDict := GetGBKHashStringDict(@C_MiniKeyDictPackageBuffer[0], SizeOf(T_MiniKeyDict_PackageBuffer), 200 * 10000);

  // big word
  bigWordDict := GetGBKHashDict(@C_miniDictPackageBuffer[0], SizeOf(T_miniDict_PackageBuffer), 200 * 10000);
end;

procedure FreeGBKMedia;
begin
  DisposeObject([WordPartDict]);
  DisposeObject([WillVecDict, WordVecDict]);
  DisposeObject([BadEmotionDict, BadRepDict, GoodEmotionDict, GoodRepDict]);
  DisposeObject([CharDict, PYDict, s2tDict, t2hkDict, t2sDict, t2twDict]);
  DisposeObject(bigKeyDict);
  DisposeObject(bigWordDict);
end;

initialization

InitGBKMedia;

finalization

FreeGBKMedia;

end. 
 
 
