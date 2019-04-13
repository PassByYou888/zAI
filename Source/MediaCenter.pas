{ ****************************************************************************** }
{ * media center                                                               * }
{ * written by QQ 600585@qq.com                                                * }
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
unit MediaCenter;

{$INCLUDE zDefine.inc}

interface

uses Classes, Types,
  CoreClasses, PascalStrings,
  LibraryManager, UnicodeMixedLib,
  ObjectDataManager, MemoryStream64, TextDataEngine, ListEngine, StreamList,
  DataFrameEngine, ObjectData, ItemStream,
  zSound,

  Geometry2DUnit;

type
  PSearchConfigInfo = ^TSearchConfigInfo;

  TSearchConfigInfo = record
    Recursion: Boolean;
    Intf: TCoreClassObject;
    Info: SystemString;
    Alias: THashVariantList;
  end;

  TFileIOHook = class
  private
    FCritical: TCritical;
    FList: TCoreClassList;

    function GetSearchItems(index: Integer): PSearchConfigInfo;
    procedure SetSearchItems(index: Integer; const Value: PSearchConfigInfo);

    function SearchObjData(aRootDir, AName: SystemString; Recursion: Boolean; aIntf: TObjectDataManager; var Hnd: TCoreClassStream): Boolean;
    function SearchLib(LibName, ItmName: SystemString; Intf: TLibraryManager; var Hnd: TCoreClassStream): Boolean;
    function SearchStreamList(AName: SystemString; Intf: THashStreamList; var Hnd: TCoreClassStream): Boolean;

    function ExistsObjData(aRootDir, AName: SystemString; Recursion: Boolean; aIntf: TObjectDataManager): Boolean;
    function ExistsLib(LibName, ItmName: SystemString; Intf: TLibraryManager): Boolean;
    function ExistsStreamList(AName: SystemString; Intf: THashStreamList): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function FileIOStream(FileName: SystemString; Mode: Word): TCoreClassStream;
    function FileIOStreamExists(FileName: SystemString): Boolean;

    property SearchItems[index: Integer]: PSearchConfigInfo read GetSearchItems write SetSearchItems; default;

    procedure Clear;
    function SearchCount: Integer;

    procedure AddPrioritySearchObj(Recursion: Boolean; Intf: TCoreClassObject; Info: SystemString);
    procedure AddSearchObj(Recursion: Boolean; Intf: TCoreClassObject; Info: SystemString);
    procedure DeleteSearchObj(Value: TCoreClassObject);
    procedure DeleteSearchIndex(Value: Integer);
    function ChangePrioritySearchOption(Intf: TCoreClassObject; Recursion: Boolean; Info: SystemString): Boolean;

    function SetSearchObjAlias(Intf: TCoreClassObject; SourFileName, DestFileName: SystemString): Boolean;
    function SetSearchObjAliasFromList(Intf: TCoreClassObject; Alias: THashVariantList): Boolean;
    function SetSearchObjAliasFromStream(Intf: TCoreClassObject; Alias: TCoreClassStream): Boolean;
  end;

var
  FileIO: TFileIOHook = nil;

  // resource: sound.ox or sound
  SoundLibrary: TLibraryManager = nil;
  // resource: art.ox or art
  ArtLibrary: TLibraryManager = nil;
  // resource: tile.ox or tile
  TileLibrary: TLibraryManager = nil;
  // resource: brush.ox or brush
  BrushLibrary: TLibraryManager = nil;
  // resource: user.ox or user
  UserLibrary: TLibraryManager = nil;

  // default sound engine
  Media: TzSound = nil;

function FileIOCreate(const FileName: SystemString): TCoreClassStream;
function FileIOOpen(const FileName: SystemString): TCoreClassStream;
function FileIOExists(const FileName: SystemString): Boolean;

function GetResourceStream(const FileName: SystemString): TStream;

type
  TGlobalMediaType = (gmtSound, gmtArt, gmtTile, gmtBrush, gmtUser);
  TGlobalMediaTypes = set of TGlobalMediaType;

const
  AllGlobalMediaTypes: TGlobalMediaTypes = ([gmtSound, gmtArt, gmtTile, gmtBrush, gmtUser]);

procedure InitGlobalMedia(t: TGlobalMediaTypes);
procedure FreeGlobalMedia;

implementation

{$IFDEF FPC}


uses SysUtils, Variants;
{$ELSE}


uses IOUtils, SysUtils, Variants;
{$ENDIF}


function FileIOCreate(const FileName: SystemString): TCoreClassStream;
begin
  FileIO.FCritical.Acquire;
  try
      Result := FileIO.FileIOStream(FileName, fmCreate);
  finally
      FileIO.FCritical.Release;
  end;
end;

function FileIOOpen(const FileName: SystemString): TCoreClassStream;
begin
  FileIO.FCritical.Acquire;
  try
      Result := FileIO.FileIOStream(FileName, fmOpenRead);
  finally
      FileIO.FCritical.Release;
  end;
end;

function FileIOExists(const FileName: SystemString): Boolean;
begin
  FileIO.FCritical.Acquire;
  try
      Result := FileIO.FileIOStreamExists(FileName);
  finally
      FileIO.FCritical.Release;
  end;
end;

function TFileIOHook.GetSearchItems(index: Integer): PSearchConfigInfo;
begin
  Result := FList[index];
end;

procedure TFileIOHook.SetSearchItems(index: Integer; const Value: PSearchConfigInfo);
begin
  FList[index] := Value;
end;

function TFileIOHook.SearchObjData(aRootDir, AName: SystemString; Recursion: Boolean; aIntf: TObjectDataManager; var Hnd: TCoreClassStream): Boolean;
var
  ItemHnd: TItemHandle;
  ItmSearchHnd: TItemSearch;
  ItmRecursionHnd: TItemRecursionSearch;
begin
  Hnd := nil;
  Result := False;
  if Recursion then
    begin
      if aIntf.RecursionSearchFirst(aRootDir, AName, ItmRecursionHnd) then
        begin
          repeat
            case ItmRecursionHnd.ReturnHeader.ID of
              db_Header_Item_ID:
                begin
                  if aIntf.ItemFastOpen(ItmRecursionHnd.ReturnHeader.CurrentHeader, ItemHnd) then
                    begin
                      Hnd := TItemStream.Create(aIntf, ItemHnd);
                      Result := True;
                      Exit;
                    end;
                end;
            end;
          until not aIntf.RecursionSearchNext(ItmRecursionHnd);
        end;
    end
  else if aIntf.ItemFindFirst(aRootDir, AName, ItmSearchHnd) then
    begin
      if aIntf.ItemFastOpen(ItmSearchHnd.HeaderPOS, ItemHnd) then
        begin
          Hnd := TItemStream.Create(aIntf, ItemHnd);
          Result := True;
          Exit;
        end;
    end;
end;

function TFileIOHook.SearchLib(LibName, ItmName: SystemString; Intf: TLibraryManager; var Hnd: TCoreClassStream): Boolean;
var
  n: SystemString;
  p: PHashStreamListData;
begin
  Hnd := nil;
  Result := False;
  if LibName = '' then
      n := ItmName
  else
      n := LibName + ':' + ItmName;
  p := Intf.PathItems[n];
  if p <> nil then
    begin
      Hnd := TItemStream.Create(Intf.DBEngine, p^.ItemHnd);
      Result := True;
    end;
end;

function TFileIOHook.SearchStreamList(AName: SystemString; Intf: THashStreamList; var Hnd: TCoreClassStream): Boolean;
var
  p: PHashStreamListData;
begin
  Hnd := nil;
  Result := False;
  p := Intf.Names[AName];
  if p <> nil then
    begin
      Hnd := TItemStream.Create(Intf.DBEngine, p^.ItemHnd);
      Result := True;
    end;
end;

function TFileIOHook.ExistsObjData(aRootDir, AName: SystemString; Recursion: Boolean; aIntf: TObjectDataManager): Boolean;
var
  ItmSearchHnd: TItemSearch;
  ItmRecursionHnd: TItemRecursionSearch;
begin
  Result := False;
  if Recursion then
    begin
      if aIntf.RecursionSearchFirst(aRootDir, AName, ItmRecursionHnd) then
        begin
          repeat
            case ItmRecursionHnd.ReturnHeader.ID of
              db_Header_Item_ID:
                begin
                  Result := True;
                  Exit;
                end;
            end;
          until not aIntf.RecursionSearchNext(ItmRecursionHnd);
        end;
    end
  else
      Result := aIntf.ItemFindFirst(aRootDir, AName, ItmSearchHnd);
end;

function TFileIOHook.ExistsLib(LibName, ItmName: SystemString; Intf: TLibraryManager): Boolean;
var
  n: SystemString;
begin
  if LibName = '' then
      n := ItmName
  else
      n := LibName + ':' + ItmName;
  Result := Intf.PathItems[n] <> nil;
end;

function TFileIOHook.ExistsStreamList(AName: SystemString; Intf: THashStreamList): Boolean;
begin
  Result := Intf.Names[AName] <> nil;
end;

constructor TFileIOHook.Create;
begin
  inherited Create;
  FCritical := TCritical.Create;
  FList := TCoreClassList.Create;
end;

destructor TFileIOHook.Destroy;
begin
  while SearchCount > 0 do
      DeleteSearchIndex(0);

  DisposeObject(FList);
  DisposeObject(FCritical);
  inherited Destroy;
end;

function TFileIOHook.FileIOStream(FileName: SystemString; Mode: Word): TCoreClassStream;
var
  i: Integer;
  p: PSearchConfigInfo;
  n: SystemString;
begin
  FileName := umlTrimSpace(FileName);
  if FileName = '' then
    begin
      Result := nil;
      Exit;
    end;

  if (Mode = fmCreate) then
    begin
      Result := TCoreClassFileStream.Create(FileName, fmCreate);
      Exit;
    end;

  n := umlGetFileName(FileName);
  Result := nil;

  if (FList.Count > 0) then
    begin
      for i := 0 to FList.Count - 1 do
        begin
          p := PSearchConfigInfo(FList[i]);
          if p^.Intf is TObjectDataManager then
            begin
              if (p^.Alias <> nil) and (p^.Alias.Exists(n)) and (VarIsStr(p^.Alias[n])) and
                (SearchObjData(p^.Info, p^.Alias[n], p^.Recursion, p^.Intf as TObjectDataManager, Result)) then
                  Exit;

              if SearchObjData(p^.Info, n, p^.Recursion, p^.Intf as TObjectDataManager, Result) then
                  Exit;
            end
          else if p^.Intf is TLibraryManager then
            begin
              if (p^.Alias <> nil) and (p^.Alias.Exists(n)) and (VarIsStr(p^.Alias[n])) and
                (SearchLib(p^.Info, p^.Alias[n], p^.Intf as TLibraryManager, Result)) then
                  Exit;

              if SearchLib(p^.Info, n, p^.Intf as TLibraryManager, Result) then
                  Exit;
            end
          else if p^.Intf is THashStreamList then
            begin
              if (p^.Alias <> nil) and (p^.Alias.Exists(n)) and (VarIsStr(p^.Alias[n])) and
                (SearchStreamList(p^.Alias[n], p^.Intf as THashStreamList, Result)) then
                  Exit;

              if SearchStreamList(n, p^.Intf as THashStreamList, Result) then
                  Exit;
            end
          else if p^.Intf = nil then
            begin
              if (p^.Alias <> nil) and (p^.Alias.Exists(n)) and (VarIsStr(p^.Alias[n])) and (umlFileExists(umlCombineFileName(p^.Info, p^.Alias[n])))
              then
                begin
                  try
                    Result := TMemoryStream64.Create;
                    TMemoryStream64(Result).LoadFromFile(umlCombineFileName(p^.Info, p^.Alias[n]));
                    Result.Position := 0;
                    Exit;
                  except
                  end;
                end
              else if umlFileExists(umlCombineFileName(p^.Info, n)) then
                begin
                  try
                    Result := TMemoryStream64.Create;
                    TMemoryStream64(Result).LoadFromFile(umlCombineFileName(p^.Info, n));
                    Result.Position := 0;
                    Exit;
                  except
                  end;
                end;
            end;
        end;
    end;

  if umlFileExists(FileName) then
    begin
      Result := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      Exit;
    end;

{$IFDEF FPC}
{$ELSE}
  n := umlGetFileName(FileName);
  n := umlCombineFileName(TPath.GetLibraryPath, n);
  if umlFileExists(n) then
    begin
      Result := TCoreClassFileStream.Create(n, fmOpenRead or fmShareDenyWrite);
      Exit;
    end;

  n := umlGetFileName(FileName);
  n := umlCombineFileName(TPath.GetDocumentsPath, n);
  if umlFileExists(n) then
    begin
      Result := TCoreClassFileStream.Create(n, fmOpenRead or fmShareDenyWrite);
      Exit;
    end;

  n := umlGetFileName(FileName);
  n := umlCombineFileName(TPath.GetDownloadsPath, n);
  if umlFileExists(n) then
    begin
      Result := TCoreClassFileStream.Create(n, fmOpenRead or fmShareDenyWrite);
      Exit;
    end;

  n := umlGetFileName(FileName);
  n := umlCombineFileName(TPath.GetSharedDownloadsPath, n);
  if umlFileExists(n) then
    begin
      Result := TCoreClassFileStream.Create(n, fmOpenRead or fmShareDenyWrite);
      Exit;
    end;
{$ENDIF}
  Result := nil;
end;

function TFileIOHook.FileIOStreamExists(FileName: SystemString): Boolean;
var
  i: Integer;
  p: PSearchConfigInfo;
  n: SystemString;
begin
  FileName := umlTrimSpace(FileName);
  if FileName = '' then
    begin
      Result := False;
      Exit;
    end;

  n := umlGetFileName(FileName);
  Result := False;
  try
    if FList.Count > 0 then
      begin
        for i := 0 to FList.Count - 1 do
          begin
            p := FList[i];
            if p^.Intf is TObjectDataManager then
              begin
                Result := ((p^.Alias <> nil) and (p^.Alias.Exists(n)) and (VarIsStr(p^.Alias[n])) and
                  (ExistsObjData(p^.Info, p^.Alias[n], p^.Recursion, p^.Intf as TObjectDataManager))) or
                  (ExistsObjData(p^.Info, n, p^.Recursion, p^.Intf as TObjectDataManager));
              end
            else if p^.Intf is TLibraryManager then
              begin
                Result := ((p^.Alias <> nil) and (p^.Alias.Exists(n)) and (VarIsStr(p^.Alias[n])) and
                  (ExistsLib(p^.Info, p^.Alias[n], p^.Intf as TLibraryManager))) or (ExistsLib(p^.Info, n, p^.Intf as TLibraryManager));
              end
            else if p^.Intf is THashStreamList then
              begin
                Result := ((p^.Alias <> nil) and (p^.Alias.Exists(n)) and (VarIsStr(p^.Alias[n])) and
                  (ExistsStreamList(p^.Alias[n], p^.Intf as THashStreamList))) or (ExistsStreamList(n, p^.Intf as THashStreamList));
              end
            else if p^.Intf = nil then
              begin
                Result := ((p^.Alias <> nil) and (p^.Alias.Exists(n)) and (VarIsStr(p^.Alias[n])) and
                  (umlFileExists(umlCombineFileName(p^.Info, p^.Alias[n])))) or (umlFileExists(umlCombineFileName(p^.Info, n)));
              end;
            if Result then
                Exit;
          end;
      end;

    if umlFileExists(FileName) then
      begin
        Result := True;
        Exit;
      end;

{$IFDEF FPC}
{$ELSE}
    n := umlGetFileName(FileName);
    n := umlCombineFileName(TPath.GetLibraryPath, n);
    if umlFileExists(n) then
      begin
        Result := True;
        Exit;
      end;

    n := umlGetFileName(FileName);
    n := umlCombineFileName(TPath.GetDocumentsPath, n);
    if umlFileExists(n) then
      begin
        Result := True;
        Exit;
      end;

    n := umlGetFileName(FileName);
    n := umlCombineFileName(TPath.GetDownloadsPath, n);
    if umlFileExists(n) then
      begin
        Result := True;
        Exit;
      end;

    n := umlGetFileName(FileName);
    n := umlCombineFileName(TPath.GetSharedDownloadsPath, n);
    if umlFileExists(n) then
      begin
        Result := True;
        Exit;
      end;
{$ENDIF}
  except
  end;
end;

procedure TFileIOHook.Clear;
begin
  while SearchCount > 0 do
      DeleteSearchIndex(0);
end;

function TFileIOHook.SearchCount: Integer;
begin
  Result := FList.Count;
end;

procedure TFileIOHook.AddPrioritySearchObj(Recursion: Boolean; Intf: TCoreClassObject; Info: SystemString);
var
  p: PSearchConfigInfo;
begin
  new(p);
  p^.Recursion := Recursion;
  p^.Intf := Intf;
  p^.Info := Info;
  p^.Alias := nil;
  if FList.Count > 0 then
    begin
      FList.Insert(0, p);
    end
  else
    begin
      FList.Add(p);
    end;
end;

procedure TFileIOHook.AddSearchObj(Recursion: Boolean; Intf: TCoreClassObject; Info: SystemString);
var
  p: PSearchConfigInfo;
begin
  new(p);
  p^.Recursion := Recursion;
  p^.Intf := Intf;
  p^.Info := Info;
  p^.Alias := nil;
  FList.Add(p);
end;

procedure TFileIOHook.DeleteSearchObj(Value: TCoreClassObject);
var
  i: Integer;
  p: PSearchConfigInfo;
begin
  i := 0;
  while i < FList.Count do
    begin
      p := FList[i];
      if p^.Intf = Value then
        begin
          FList.Delete(i);
          if p^.Alias <> nil then
            begin
              try
                  DisposeObject(p^.Alias);
              except
              end;
            end;
          Dispose(p);
          Break;
        end
      else
          inc(i);
    end;
end;

procedure TFileIOHook.DeleteSearchIndex(Value: Integer);
var
  p: PSearchConfigInfo;
begin
  p := FList[Value];
  if p^.Alias <> nil then
    begin
      try
          DisposeObject(p^.Alias);
      except
      end;
    end;
  Dispose(p);
  FList.Delete(Value);
end;

function TFileIOHook.ChangePrioritySearchOption(Intf: TCoreClassObject; Recursion: Boolean; Info: SystemString): Boolean;
var
  i: Integer;
  p: PSearchConfigInfo;
begin
  Result := False;
  i := 0;
  while i < FList.Count do
    begin
      p := FList[i];
      if p^.Intf = Intf then
        begin
          p^.Recursion := Recursion;
          p^.Info := Info;
          if i <> 0 then
              FList.Move(i, 0);
          Result := True;
        end;
      inc(i);
    end;
  if not Result then
      AddPrioritySearchObj(Recursion, Intf, Info);
  Result := True;
end;

function TFileIOHook.SetSearchObjAlias(Intf: TCoreClassObject; SourFileName, DestFileName: SystemString): Boolean;
var
  i: Integer;
  p: PSearchConfigInfo;
begin
  Result := False;
  i := 0;
  while i < FList.Count do
    begin
      p := FList[i];
      if p^.Intf = Intf then
        begin
          if p^.Alias = nil then
              p^.Alias := THashVariantList.Create;

          p^.Alias[SourFileName] := DestFileName;
          Result := True;
        end;
      inc(i);
    end;
end;

function TFileIOHook.SetSearchObjAliasFromList(Intf: TCoreClassObject; Alias: THashVariantList): Boolean;
var
  i: Integer;
  p: PSearchConfigInfo;
begin
  Result := False;
  if Alias = nil then
      Exit;
  i := 0;
  while i < FList.Count do
    begin
      p := FList[i];
      if p^.Intf = Intf then
        begin
          if p^.Alias = nil then
              p^.Alias := THashVariantList.Create;
          p^.Alias.CopyFrom(Alias);
          Result := True;
        end;
      inc(i);
    end;
end;

function TFileIOHook.SetSearchObjAliasFromStream(Intf: TCoreClassObject; Alias: TCoreClassStream): Boolean;
var
  i: Integer;
  p: PSearchConfigInfo;
  ts: THashVariantTextStream;
begin
  Result := False;
  if Alias = nil then
      Exit;
  i := 0;
  while i < FList.Count do
    begin
      p := FList[i];
      if p^.Intf = Intf then
        begin
          if p^.Alias = nil then
              p^.Alias := THashVariantList.Create;
          ts := THashVariantTextStream.Create(p^.Alias);
          ts.LoadFromStream(Alias);
          DisposeObject(ts);
          Result := True;
        end;
      inc(i);
    end;
end;

function GetResourceStream(const FileName: SystemString): TStream;
var
  n: SystemString;
begin
  Result := nil;

  if TPascalString(FileName).Exists('.') then
      n := umlDeleteLastStr(FileName, '.');

{$IFDEF FPC}
  if FindResource(HInstance, n, RT_RCDATA) = 0 then
{$ELSE}
  if FindResource(HInstance, PChar(n), RT_RCDATA) = 0 then
{$ENDIF}
    begin
{$IFDEF FPC}
{$ELSE}
      n := umlGetFileName(FileName);
      n := umlCombineFileName(TPath.GetLibraryPath, n);
      if umlFileExists(n) then
        begin
          Result := TCoreClassFileStream.Create(n, fmOpenRead or fmShareDenyWrite);
          Exit;
        end;

      n := umlGetFileName(FileName);
      n := umlCombineFileName(TPath.GetDocumentsPath, n);
      if umlFileExists(n) then
        begin
          Result := TCoreClassFileStream.Create(n, fmOpenRead or fmShareDenyWrite);
          Exit;
        end;

      n := umlGetFileName(FileName);
      n := umlCombineFileName(TPath.GetDownloadsPath, n);
      if umlFileExists(n) then
        begin
          Result := TCoreClassFileStream.Create(n, fmOpenRead or fmShareDenyWrite);
          Exit;
        end;

      n := umlGetFileName(FileName);
      n := umlCombineFileName(TPath.GetSharedDownloadsPath, n);
      if umlFileExists(n) then
        begin
          Result := TCoreClassFileStream.Create(n, fmOpenRead or fmShareDenyWrite);
          Exit;
        end;
{$ENDIF}
      n := umlGetFileName(FileName);
      if FileIOExists(n) then
        begin
          Result := FileIOOpen(n);
          Exit;
        end;

      RaiseInfo('no exists resource file "%s"', [n]);
    end
  else
    begin
      Result := TResourceStream.Create(HInstance, n, RT_RCDATA);
    end;
end;

procedure InitGlobalMedia(t: TGlobalMediaTypes);
var
  db: TObjectDataManager;
begin
  FreeGlobalMedia;

  if gmtSound in t then
    begin
      db := TObjectDataManager.CreateAsStream(GetResourceStream('sound.ox'), 'sound.ox', ObjectDataMarshal.ID, True, False, True);
      SoundLibrary := TLibraryManager.Create(db, '/');
      SoundLibrary.AutoFreeDataEngine := True;
      FileIO.AddSearchObj(True, SoundLibrary, '/');
    end;

  if gmtArt in t then
    begin
      db := TObjectDataManager.CreateAsStream(GetResourceStream('art.ox'), 'art.ox', ObjectDataMarshal.ID, True, False, True);
      ArtLibrary := TLibraryManager.Create(db, '/');
      ArtLibrary.AutoFreeDataEngine := True;
      FileIO.AddSearchObj(True, ArtLibrary, '/');
    end;

  if gmtTile in t then
    begin
      db := TObjectDataManager.CreateAsStream(GetResourceStream('tile.ox'), 'tile.ox', ObjectDataMarshal.ID, True, False, True);
      TileLibrary := TLibraryManager.Create(db, '/');
      TileLibrary.AutoFreeDataEngine := True;
      FileIO.AddSearchObj(True, TileLibrary, '/');
    end;

  if gmtBrush in t then
    begin
      db := TObjectDataManager.CreateAsStream(GetResourceStream('brush.ox'), 'brush.ox', ObjectDataMarshal.ID, True, False, True);
      BrushLibrary := TLibraryManager.Create(db, '/');
      BrushLibrary.AutoFreeDataEngine := True;
      FileIO.AddSearchObj(True, BrushLibrary, '/');
    end;

  if gmtUser in t then
    begin
      db := TObjectDataManager.CreateAsStream(GetResourceStream('user.ox'), 'user.ox', ObjectDataMarshal.ID, True, False, True);
      UserLibrary := TLibraryManager.Create(db, '/');
      UserLibrary.AutoFreeDataEngine := True;
      FileIO.AddSearchObj(True, UserLibrary, '/');
    end;

{$IFDEF FPC}
  Media := DefaultSoundEngineClass.Create(umlCurrentPath);
{$ELSE}
  Media := DefaultSoundEngineClass.Create(TPath.GetTempPath);
{$ENDIF}
  Media.SearchDB := SoundLibrary;

end;

procedure FreeGlobalMedia;
begin
  if SoundLibrary <> nil then
    begin
      FileIO.DeleteSearchObj(SoundLibrary);
      DisposeObject(SoundLibrary);
      SoundLibrary := nil;
    end;

  if ArtLibrary <> nil then
    begin
      FileIO.DeleteSearchObj(ArtLibrary);
      DisposeObject(ArtLibrary);
      ArtLibrary := nil;
    end;

  if TileLibrary <> nil then
    begin
      FileIO.DeleteSearchObj(TileLibrary);
      DisposeObject(TileLibrary);
      TileLibrary := nil;
    end;

  if BrushLibrary <> nil then
    begin
      FileIO.DeleteSearchObj(BrushLibrary);
      DisposeObject(BrushLibrary);
      BrushLibrary := nil;
    end;

  if UserLibrary <> nil then
    begin
      FileIO.DeleteSearchObj(UserLibrary);
      DisposeObject(UserLibrary);
      UserLibrary := nil;
    end;

  if Media <> nil then
    begin
      DisposeObject(Media);
      Media := nil;
    end;
end;

initialization

FileIO := TFileIOHook.Create;

SoundLibrary := nil;
ArtLibrary := nil;
TileLibrary := nil;
BrushLibrary := nil;
UserLibrary := nil;
Media := nil;

finalization


FreeGlobalMedia;
if FileIO <> nil then
  begin
    DisposeObject(FileIO);
    FileIO := nil;
  end;

end.
