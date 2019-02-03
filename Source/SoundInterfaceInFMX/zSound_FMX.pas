{ ****************************************************************************** }
{ * sound engine with FMX Support                                              * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ ****************************************************************************** }
unit zSound_FMX;

{$INCLUDE ..\zDefine.inc}

interface

uses CoreClasses, zSound, UnicodeMixedLib, MediaCenter,
  ObjectDataManager, ItemStream, LibraryManager, ListEngine, PascalStrings, MemoryStream64,

  FMX.Media,

  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Dialogs, FMX.Forms
  {$IFDEF ANDROID}
    , AndroidApi.JNI.Media, FMX.Helpers.Android, AndroidApi.JNI.JavaTypes, AndroidApi.JNI.GraphicsContentViewText, AndroidApi.JNIBridge,
  AndroidApi.Helpers, System.Threading
  {$ENDIF}
  {$IFDEF IOS}
    , Macapi.CoreFoundation, FMX.Platform.iOS, iOSApi.CocoaTypes, iOSApi.AVFoundation, iOSApi.Foundation
  {$ELSE}
  {$IFDEF MACOS}
    , Macapi.CoreFoundation, FMX.Platform.Mac, Macapi.CocoaTypes, Macapi.AppKit, Macapi.Foundation, Macapi.Helpers
  {$ENDIF}
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    , MMSystem
  {$ENDIF}
    ;

type
  TSoundRec = record
    SFilename: SystemString;
    SName: SystemString;
    SNameExt: SystemString;
    SID: Integer;
  end;

  PSoundRec = ^TSoundRec;

  TAudioManager = class
  private
    fSoundsList    : TCoreClassList;
    fSoundsHashList: THashVariantList;

    {$IFDEF ANDROID}
    fAudioMgr : JAudioManager;
    fSoundPool: JSoundPool;
    {$ENDIF}
    function GetSoundsCount: Integer;
    function GetSoundFromIndex(aIndex: Integer): PSoundRec;
  public
    constructor Create;
    destructor Destroy; override;

    function AddSound(ASoundFile: SystemString): Integer;
    procedure DeleteSound(aIndex: Integer); overload;
    procedure PlaySound(aIndex: Integer); overload;

    procedure DeleteSound(aFile: SystemString); overload;
    procedure PlaySound(aFile: SystemString); overload;

    function Exists(aFile: SystemString): Boolean;

    property SoundsCount: Integer read GetSoundsCount;
    property Sounds[aIndex: Integer]: PSoundRec read GetSoundFromIndex;
  end;

  TSEBase = class
  protected
    function GetCurrentTime: TMediaTime; virtual; abstract;
    procedure SetCurrentTime(const Value: TMediaTime); virtual; abstract;

    function GetFileName: SystemString; virtual; abstract;
    procedure SetFileName(const Value: SystemString); virtual; abstract;
  public
    procedure Play; virtual; abstract;
    procedure stop; virtual; abstract;

    function Playing: Boolean; virtual; abstract;

    property CurrentTime: TMediaTime read GetCurrentTime write SetCurrentTime;
    property FileName: SystemString read GetFileName write SetFileName;
  end;

  TSEMediaPlayer = class(TSEBase)
  protected
    Media: TMediaPlayer;
    function GetCurrentTime: TMediaTime; override;
    procedure SetCurrentTime(const Value: TMediaTime); override;

    function GetFileName: SystemString; override;
    procedure SetFileName(const Value: SystemString); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Play; override;
    procedure stop; override;

    function Playing: Boolean; override;
  end;

  TSoundEngine_FMX = class(TzSound)
  protected
    FMediaMusic  : TSEBase;
    FMediaAmbient: TSEBase;

    FMusicPlaying  : Boolean;
    FAmbientPlaying: Boolean;

    procedure DoPrepareMusic(FileName: SystemString); override;
    procedure DoPlayMusic(FileName: SystemString); override;
    procedure DoStopMusic; override;

    procedure DoPrepareAmbient(FileName: SystemString); override;
    procedure DoPlayAmbient(FileName: SystemString); override;
    procedure DoStopAmbient; override;

    procedure DoPrepareSound(FileName: SystemString); override;
    procedure DoPlaySound(FileName: SystemString); override;
    procedure DoStopSound(FileName: SystemString); override;

    procedure DoStopAll; override;

    function SaveSoundAsLocalFile(FileName: SystemString): SystemString; override;
    function SoundReadyOk(FileName: SystemString): Boolean; override;

    function MediaIsPlaying(M: TSEBase): Boolean;
  public
    constructor Create(ATempPath: SystemString); override;
    destructor Destroy; override;

    procedure Progress(deltaTime: Double); override;
  end;

var
  SoundEngine_FMX: TSoundEngine_FMX = nil;

implementation

var
  AudioManager: TAudioManager;

  {$IFDEF IOS}


const
  _libAudioToolbox = '/System/Library/Frameworks/AudioToolbox.framework/AudioToolbox';

procedure AudioServicesPlaySystemSound(inSystemSoundID: nsinteger); cdecl; external _libAudioToolbox Name 'AudioServicesPlaySystemSound';
procedure AudioServicesCreateSystemSoundID(inFileURL: CFURLRef; var SystemSoundID: pnsinteger); cdecl; external _libAudioToolbox Name 'AudioServicesCreateSystemSoundID';
procedure AudioServicesDisposeSystemSoundID(inSystemSoundID: nsinteger); cdecl; external _libAudioToolbox Name 'AudioServicesDisposeSystemSoundID';
procedure AudioServicesAddSystemSoundCompletion(inSystemSoundID: nsinteger; inRunLoop: CFRunLoopRef; inRunLoopMode: CFStringRef; inCompletionRoutine: Pointer; inClientData: CFURLRef); cdecl;
  external _libAudioToolbox Name 'AudioServicesAddSystemSoundCompletion';
{$ENDIF}

{ TAudioManager }

{$IF Defined(IOS) OR Defined(MACOS)}


procedure oncompleteionIosProc(SystemSndID: nsinteger; var aData: Pointer);
begin
  // place here the code to run when a sound finish playing
end;
{$ENDIF}


constructor TAudioManager.Create;
begin
  try
    fSoundsList := TCoreClassList.Create;
    fSoundsHashList := THashVariantList.Create;

    {$IFDEF ANDROID}
    fAudioMgr := TJAudioManager.Wrap((SharedActivity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE) as ILocalObject).GetObjectID);
    fSoundPool := TJSoundPool.JavaClass.Init(4, TJAudioManager.JavaClass.STREAM_MUSIC, 0);
    {$ENDIF}
  except
    on E: Exception do
        raise Exception.Create('[TAudioManager.Create] : ' + E.Message);
  end;
end;

destructor TAudioManager.Destroy;
var
  i   : Integer;
  wRec: PSoundRec;
begin
  try
    for i := fSoundsList.Count - 1 downto 0 do
      begin
        wRec := fSoundsList[i];
        Dispose(wRec);
        fSoundsList.Delete(i);
      end;
    DisposeObject([fSoundsList, fSoundsHashList]);

    {$IFDEF ANDROID}
    fSoundPool := nil;
    fAudioMgr := nil;
    {$ENDIF}
    inherited Destroy;
  except
    on E: Exception do
        raise Exception.Create('[TAudioManager.Destroy] : ' + E.Message);
  end;
end;

function TAudioManager.AddSound(ASoundFile: SystemString): Integer;
var
  wSndRec: PSoundRec;
  {$IFDEF ANDROID}
  wOnAndroidSndComplete: JSoundPool_OnLoadCompleteListener;
  soundID              : NativeInt;
  {$ENDIF}
  {$IFDEF IOS}
  wSndID        : nsinteger;
  wNSFilename   : CFStringRef;
  wNSURL        : CFURLRef;
  wCFRunLoopRef : CFRunLoopRef;
  winRunLoopMode: CFStringRef;
  {$ENDIF}
begin
  Result := -1;
  try
    new(wSndRec);
    wSndRec.SFilename := ASoundFile;
    wSndRec.SNameExt := ExtractFilename(ASoundFile);
    wSndRec.SName := ChangeFileExt(wSndRec.SNameExt, '');

    {$IFDEF ANDROID}
    wSndRec.SID := fSoundPool.load(StringToJString(ASoundFile), 0);
    {$ENDIF}
    {$IFDEF IOS}
    wNSFilename := CFStringCreateWithCharacters(nil, PChar(ASoundFile), length(ASoundFile));
    wNSURL := CFURLCreateWithFileSystemPath(nil, wNSFilename, kCFURLPOSIXPathStyle, False);
    AudioServicesCreateSystemSoundID(wNSURL, pnsinteger(wSndID));
    wSndRec.SID := wSndID;
    AudioServicesAddSystemSoundCompletion(wSndID, nil, nil, @oncompleteionIosProc, nil);
    {$ENDIF}
    Result := fSoundsList.Add(wSndRec);
    fSoundsHashList.Add(umlGetFileName(ASoundFile), Result);
  except
    on E: Exception do
        raise Exception.Create('[TAudioManager.AddSound] : ' + E.Message);
  end;
end;

procedure TAudioManager.DeleteSound(aIndex: Integer);
var
  wRec: PSoundRec;
begin
  try
    if aIndex < fSoundsList.Count then
      begin
        wRec := fSoundsList[aIndex];
        {$IFDEF ANDROID}
        fSoundPool.unload(wRec.SID);
        {$ENDIF}
        {$IFDEF IOS}
        AudioServicesDisposeSystemSoundID(wRec.SID);
        {$ENDIF}
        Dispose(wRec);
        fSoundsList.Delete(aIndex);
      end;
  except
    on E: Exception do
        raise Exception.Create('[TAudioManager.DeleteSound] : ' + E.Message);
  end;
end;

procedure TAudioManager.PlaySound(aIndex: Integer);
var
  wRec: PSoundRec;
  {$IFDEF ANDROID}
  wCurrVolume, wMaxVolume: Double;
  wVolume                : Double;
  {$ENDIF}
  {$IFNDEF IOS}
  {$IFDEF MACOS}
  wNssound: NSSound;
  {$ENDIF}
  {$ENDIF}
begin
  try
    if aIndex < fSoundsList.Count then
      begin
        wRec := fSoundsList[aIndex];
        {$IFDEF ANDROID}
        if Assigned(fAudioMgr) then
          begin
            wCurrVolume := fAudioMgr.getStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
            wMaxVolume := fAudioMgr.getStreamMaxVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
            wVolume := wCurrVolume / wMaxVolume;
            fSoundPool.Play(wRec.SID, wVolume, wVolume, 1, 0, 1);
          end;
        {$ENDIF}
        {$IFDEF IOS}
        AudioServicesAddSystemSoundCompletion(wRec.SID, nil, nil, @oncompleteionIosProc, nil);
        AudioServicesPlaySystemSound(wRec.SID);
        {$ELSE}
        {$IFDEF MACOS}
        wNssound := TNSSound.Wrap(TNSSound.Alloc.initWithContentsOfFile(StrToNSStr(wRec.SFilename), True));
        try
          wNssound.setLoops(False);
          wNssound.Play;
        finally
            wNssound.Release;
        end;
        {$ENDIF}
        {$ENDIF}
        {$IFDEF MSWINDOWS}
        sndPlaySound(PChar(wRec.SFilename), SND_NODEFAULT or SND_ASYNC);
        {$ENDIF}
      end;
  except
    on E: Exception do
        raise Exception.Create('[Unknown Name] : ' + E.Message);
  end;
end;

procedure TAudioManager.DeleteSound(aFile: SystemString);
begin
  if not Exists(aFile) then
      Exit;
  DeleteSound(Integer(fSoundsHashList[umlGetFileName(aFile)]));
  fSoundsHashList.Delete(umlGetFileName(aFile));
end;

procedure TAudioManager.PlaySound(aFile: SystemString);
begin
  if not Exists(aFile) then
      Exit;
  PlaySound(Integer(fSoundsHashList[umlGetFileName(aFile)]));
end;

function TAudioManager.Exists(aFile: SystemString): Boolean;
begin
  Result := fSoundsHashList.Exists(umlGetFileName(aFile));
end;

function TAudioManager.GetSoundsCount: Integer;
begin
  Result := fSoundsList.Count;
end;

function TAudioManager.GetSoundFromIndex(aIndex: Integer): PSoundRec;
begin
  if aIndex < fSoundsList.Count then
      Result := fSoundsList[aIndex]
  else
      Result := nil;
end;

function TSEMediaPlayer.GetCurrentTime: TMediaTime;
begin
  Result := Media.CurrentTime;
end;

procedure TSEMediaPlayer.SetCurrentTime(const Value: TMediaTime);
begin
  Media.CurrentTime := Value;
end;

function TSEMediaPlayer.GetFileName: SystemString;
begin
  Result := Media.FileName;
end;

procedure TSEMediaPlayer.SetFileName(const Value: SystemString);
begin
  Media.FileName := Value;
end;

constructor TSEMediaPlayer.Create;
begin
  inherited Create;
  Media := TMediaPlayer.Create(nil);
end;

destructor TSEMediaPlayer.Destroy;
begin
  DisposeObject(Media);
  inherited Destroy;
end;

procedure TSEMediaPlayer.Play;
begin
  Media.Play;
end;

procedure TSEMediaPlayer.stop;
begin
  Media.stop;
end;

function TSEMediaPlayer.Playing: Boolean;
begin
  Result := (Media.State = TMediaState.Playing) and (Media.Duration <> Media.CurrentTime);
end;

procedure TSoundEngine_FMX.DoPrepareMusic(FileName: SystemString);
begin
end;

procedure TSoundEngine_FMX.DoPlayMusic(FileName: SystemString);
begin
  if not umlFileExists(FileName) then
      RaiseInfo('no media file:%s', [FileName]);
  if not SameText(FMediaMusic.FileName, FileName) then
      FMediaMusic.FileName := FileName
  else
      FMediaMusic.CurrentTime := 0;
  FMediaMusic.Play;
  FMusicPlaying := True;
end;

procedure TSoundEngine_FMX.DoStopMusic;
begin
  FMediaMusic.stop;
  FMusicPlaying := False;
end;

procedure TSoundEngine_FMX.DoPrepareAmbient(FileName: SystemString);
begin
end;

procedure TSoundEngine_FMX.DoPlayAmbient(FileName: SystemString);
begin
  if not umlFileExists(FileName) then
      RaiseInfo('no media file:%s', [FileName]);
  if not SameText(FMediaAmbient.FileName, FileName) then
      FMediaAmbient.FileName := FileName
  else
      FMediaAmbient.CurrentTime := 0;
  FMediaAmbient.Play;
  FAmbientPlaying := True;
end;

procedure TSoundEngine_FMX.DoStopAmbient;
begin
  FMediaAmbient.stop;
  FAmbientPlaying := False;
end;

procedure TSoundEngine_FMX.DoPrepareSound(FileName: SystemString);
begin
  if not AudioManager.Exists(FileName) then
      AudioManager.AddSound(FileName);
end;

procedure TSoundEngine_FMX.DoPlaySound(FileName: SystemString);
begin
  DoPrepareSound(FileName);
  AudioManager.PlaySound(FileName);
end;

procedure TSoundEngine_FMX.DoStopSound(FileName: SystemString);
begin
end;

procedure TSoundEngine_FMX.DoStopAll;
begin
  FMediaMusic.stop;
  FMediaAmbient.stop;
  FMusicPlaying := False;
  FAmbientPlaying := False;
end;

function TSoundEngine_FMX.SaveSoundAsLocalFile(FileName: SystemString): SystemString;
var
  SourStream: TCoreClassStream;
  m64       : TMemoryStream64;
  md5name   : SystemString;
begin
  Result := '';
  if FCacheFileList.Exists(FileName) then
    begin
      Result := FCacheFileList[FileName];
      if umlFileExists(Result) then
          Exit;
    end;

  FileIO.ChangePrioritySearchOption(SearchDB, True, '');
  SourStream := FileIOOpen(FileName);
  if SourStream = nil then
      RaiseInfo('no sound file: "%s"', [FileName]);
  SourStream.Position := 0;
  m64 := TMemoryStream64.Create;
  m64.CopyFrom(SourStream, SourStream.Size);
  DisposeObject(SourStream);
  m64.Position := 0;
  md5name := umlStreamMD5Char(m64).Text;

  Result := umlCombineFileName(FTempPath, md5name + umlGetFileExt(FileName));
  try
      m64.SaveToFile(Result);
  except
  end;
  DisposeObject(m64);

  FCacheFileList[FileName] := Result;
end;

function TSoundEngine_FMX.SoundReadyOk(FileName: SystemString): Boolean;
begin
  Result := umlFileExists(FileName);
end;

function TSoundEngine_FMX.MediaIsPlaying(M: TSEBase): Boolean;
begin
  Result := M.Playing;
end;

constructor TSoundEngine_FMX.Create(ATempPath: SystemString);
var
  i: Integer;
begin
  inherited Create(ATempPath);
  FMediaMusic := TSEMediaPlayer.Create;
  FMediaAmbient := TSEMediaPlayer.Create;

  FMusicPlaying := False;
  FAmbientPlaying := False;

  SoundEngine_FMX := Self;
end;

destructor TSoundEngine_FMX.Destroy;
var
  i: Integer;
begin
  StopAll;
  DisposeObject(FMediaMusic);
  FMediaMusic := nil;
  DisposeObject(FMediaAmbient);
  FMediaAmbient := nil;
  SoundEngine_FMX := nil;
  inherited Destroy;
end;

procedure TSoundEngine_FMX.Progress(deltaTime: Double);
var
  i: Integer;
begin
  inherited Progress(deltaTime);

  try
    if (FMusicPlaying) and (not MediaIsPlaying(FMediaMusic)) then
      begin
        try
          if FMediaMusic.CurrentTime <> 0 then
              FMediaMusic.CurrentTime := 0;
          FMediaMusic.Play;
        except
        end;
      end;
    if (FAmbientPlaying) and (not MediaIsPlaying(FMediaAmbient)) then
      begin
        try
          if FMediaAmbient.CurrentTime <> 0 then
              FMediaAmbient.CurrentTime := 0;
          FMediaAmbient.Play;
        except
        end;
      end;
  except
  end;
end;

initialization

DefaultSoundEngineClass := TSoundEngine_FMX;
AudioManager := TAudioManager.Create;

finalization

DisposeObject(AudioManager);

end. 
 
 
 
