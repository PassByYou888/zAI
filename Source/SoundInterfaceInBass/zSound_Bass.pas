{ ****************************************************************************** }
{ * sound engine for BASS                                                      * }
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
unit zSound_Bass;

{$INCLUDE ..\zDefine.inc}

interface

uses CoreClasses, zSound, UnicodeMixedLib, MediaCenter,
  ObjectDataManager, ItemStream, ObjectDataHashField, ListEngine, PascalStrings, MemoryStream64, Bass;

type
  TSoundEngine_Bass = class(TzSound)
  protected
    SoundList: THashObjectList;
    FLastPlayChannel: Cardinal;

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

    function DoIsPlaying(FileName: SystemString): Boolean; override;

    function SaveSoundAsLocalFile(FileName: SystemString): SystemString; override;
    function SoundReadyOk(FileName: SystemString): Boolean; override;
  public
    constructor Create(ATempPath: SystemString); override;
    destructor Destroy; override;

    procedure Progress(deltaTime: Double); override;
    property LastPlayChannel: Cardinal read FLastPlayChannel;
  end;

var
  SoundEngine_Bass: TSoundEngine_Bass = nil;

implementation

type
  TSoundStyle = (ssMusic, ssAmbient, ssSound, ssUnknow);

  TSound = class
  public
    Owner: TSoundEngine_Bass;
    Name: SystemString;
    Handle: Cardinal;
    Style: TSoundStyle;
    channel: Cardinal;

    constructor Create;
    destructor Destroy; override;
  end;

constructor TSound.Create;
begin
  Owner := nil;
  Name := '';
  Handle := 0;
  Style := ssUnknow;
  channel := 0;
  inherited Create;
end;

destructor TSound.Destroy;
begin
  case Style of
    ssMusic: BASS_MusicFree(Handle);
    ssAmbient: BASS_MusicFree(Handle);
    ssSound: BASS_SampleFree(Handle);
  end;
  inherited Destroy;
end;

procedure TSoundEngine_Bass.DoPrepareMusic(FileName: SystemString);
var
  s: TSound;
  stream: TCoreClassStream;
  ms: TMemoryStream64;
begin
  if SoundList.Exists(FileName) then
      exit;

  stream := FileIOOpen(FileName);
  if stream = nil then
      exit;

  ms := TMemoryStream64.Create;
  ms.CopyFrom(stream, stream.Size);
  ms.Position := 0;

  s := TSound.Create;
  s.Owner := Self;
  s.Name := FileName;
  s.Handle := BASS_SampleLoad(True, ms.Memory, 0, ms.Size, 3, BASS_SAMPLE_LOOP or BASS_SAMPLE_OVER_POS {$IFDEF UNICODE} or BASS_UNICODE{$ENDIF});
  s.Style := ssMusic;
  s.channel := 0;

  DisposeObject([ms, stream]);

  if s.Handle <> 0 then
    begin
      SoundList[FileName] := s;
      exit;
    end;
  DisposeObject(s);
end;

procedure TSoundEngine_Bass.DoPlayMusic(FileName: SystemString);
var
  s: TSound;
begin
  DoStopMusic;

  s := SoundList[FileName] as TSound;
  if s = nil then
    begin
      DoPrepareMusic(FileName);
      s := SoundList[FileName] as TSound;
    end;

  if s = nil then
      exit;

  s.channel := BASS_SampleGetChannel(s.Handle, false);
  if s.channel <> 0 then
    begin
      BASS_ChannelPlay(s.channel, false);
      FLastPlayChannel := s.channel;
    end;
end;

procedure TSoundEngine_Bass.DoStopMusic;
var
  lst: TCoreClassListForObj;
  i: Integer;
  s: TSound;
begin
  lst := TCoreClassListForObj.Create;
  SoundList.GetAsList(lst);
  for i := 0 to lst.Count - 1 do
    begin
      s := lst[i] as TSound;

      if s.Style = ssMusic then
        if s.channel <> 0 then
            BASS_ChannelStop(s.channel);
    end;
  DisposeObject(lst);
end;

procedure TSoundEngine_Bass.DoPrepareAmbient(FileName: SystemString);
var
  s: TSound;
  stream: TCoreClassStream;
  ms: TMemoryStream64;
begin
  if SoundList.Exists(FileName) then
      exit;

  stream := FileIOOpen(FileName);
  if stream = nil then
      exit;

  ms := TMemoryStream64.Create;
  ms.CopyFrom(stream, stream.Size);
  ms.Position := 0;

  s := TSound.Create;
  s.Owner := Self;
  s.Name := FileName;
  s.Handle := BASS_SampleLoad(True, ms.Memory, 0, ms.Size, 5, BASS_SAMPLE_LOOP or BASS_SAMPLE_OVER_POS {$IFDEF UNICODE} or BASS_UNICODE{$ENDIF});
  s.Style := ssAmbient;
  s.channel := 0;

  DisposeObject([ms, stream]);

  if s.Handle <> 0 then
    begin
      SoundList[FileName] := s;
      exit;
    end;
  DisposeObject(s);
end;

procedure TSoundEngine_Bass.DoPlayAmbient(FileName: SystemString);
var
  s: TSound;
begin
  if DoIsPlaying(FileName) then
      exit;

  s := SoundList[FileName] as TSound;
  if s = nil then
    begin
      DoPrepareAmbient(FileName);
      s := SoundList[FileName] as TSound;
    end;

  if s = nil then
      exit;

  s.channel := BASS_SampleGetChannel(s.Handle, false);
  if s.channel <> 0 then
    begin
      BASS_ChannelPlay(s.channel, false);
      FLastPlayChannel := s.channel;
    end;
end;

procedure TSoundEngine_Bass.DoStopAmbient;
var
  lst: TCoreClassListForObj;
  i: Integer;
  s: TSound;
begin
  lst := TCoreClassListForObj.Create;
  SoundList.GetAsList(lst);
  for i := 0 to lst.Count - 1 do
    begin
      s := lst[i] as TSound;

      if s.Style = ssAmbient then
        if s.channel <> 0 then
            BASS_ChannelStop(s.channel);
    end;
  DisposeObject(lst);
end;

procedure TSoundEngine_Bass.DoPrepareSound(FileName: SystemString);
var
  s: TSound;
  stream: TCoreClassStream;
  ms: TMemoryStream64;
begin
  if SoundList.Exists(FileName) then
      exit;

  stream := FileIOOpen(FileName);
  if stream = nil then
      exit;

  ms := TMemoryStream64.Create;
  ms.CopyFrom(stream, stream.Size);
  ms.Position := 0;

  s := TSound.Create;
  s.Owner := Self;
  s.Name := FileName;
  s.Handle := BASS_SampleLoad(True, ms.Memory, 0, ms.Size, 3, BASS_SAMPLE_OVER_POS {$IFDEF UNICODE} or BASS_UNICODE{$ENDIF});
  s.Style := ssSound;
  s.channel := 0;

  DisposeObject([ms, stream]);

  if s.Handle <> 0 then
    begin
      SoundList[FileName] := s;
      exit;
    end
  else
      RaiseInfo('bass error:%d', [BASS_ErrorGetCode]);
  DisposeObject(s);
end;

procedure TSoundEngine_Bass.DoPlaySound(FileName: SystemString);
var
  s: TSound;
begin
  s := SoundList[FileName] as TSound;
  if s = nil then
    begin
      DoPrepareSound(FileName);
      s := SoundList[FileName] as TSound;
    end;

  if s = nil then
      exit;

  if (s.channel <> 0) and (BASS_ChannelIsActive(s.channel) = BASS_ACTIVE_PLAYING) then
    begin
      BASS_ChannelPlay(s.channel, True);
      FLastPlayChannel := s.channel;
    end
  else
    begin
      s.channel := BASS_SampleGetChannel(s.Handle, false);
      if s.channel <> 0 then
        begin
          BASS_ChannelPlay(s.channel, false);
          FLastPlayChannel := s.channel;
        end;
    end;
end;

procedure TSoundEngine_Bass.DoStopSound(FileName: SystemString);
var
  s: TSound;
begin
  s := SoundList[FileName] as TSound;
  if s = nil then
    begin
      DoPrepareSound(FileName);
      s := SoundList[FileName] as TSound;
    end;

  if s = nil then
      exit;

  if s.channel <> 0 then
      BASS_ChannelStop(s.channel);
end;

procedure TSoundEngine_Bass.DoStopAll;
var
  lst: TCoreClassListForObj;
  i: Integer;
  s: TSound;
begin
  lst := TCoreClassListForObj.Create;
  SoundList.GetAsList(lst);
  for i := 0 to lst.Count - 1 do
    begin
      s := lst[i] as TSound;
      if s.channel <> 0 then
          BASS_ChannelStop(s.channel);
    end;
  DisposeObject(lst);
end;

function TSoundEngine_Bass.DoIsPlaying(FileName: SystemString): Boolean;
var
  s: TSound;
begin
  Result := false;

  s := SoundList[FileName] as TSound;
  if s = nil then
    begin
      DoPrepareAmbient(FileName);
      s := SoundList[FileName] as TSound;
    end;

  if s = nil then
      exit;

  Result := (s.channel <> 0) and (BASS_ChannelIsActive(s.channel) = BASS_ACTIVE_PLAYING);
end;

function TSoundEngine_Bass.SaveSoundAsLocalFile(FileName: SystemString): SystemString;
begin
  Result := FileName;
end;

function TSoundEngine_Bass.SoundReadyOk(FileName: SystemString): Boolean;
begin
  Result := True;
end;

constructor TSoundEngine_Bass.Create(ATempPath: SystemString);
begin
  inherited Create(ATempPath);
  SoundList := THashObjectList.CustomCreate(True, 2048);
  SoundEngine_Bass := Self;
  FLastPlayChannel := 0;
end;

destructor TSoundEngine_Bass.Destroy;
begin
  StopAll;
  SoundEngine_Bass := nil;
  DisposeObject(SoundList);
  inherited Destroy;
end;

procedure TSoundEngine_Bass.Progress(deltaTime: Double);
begin
  inherited Progress(deltaTime);
end;

initialization

if not Bass_Available then
    exit;
try
{$IFDEF MSWINDOWS}
  if not BASS_Init(-1, 44100, 0, 0, nil) then
      RaiseInfo('bass init failed (%d)', [BASS_ErrorGetCode]);
{$ELSE}
  if not BASS_Init(-1, 44100, 0, nil, nil) then
      RaiseInfo('bass init failed (%d)', [BASS_ErrorGetCode]);
{$ENDIF}
  BASS_SetConfig(BASS_CONFIG_NET_PLAYLIST, 1); // enable playlist processing
  BASS_SetConfig(BASS_CONFIG_NET_READTIMEOUT, 2000);
  BASS_SetConfig(BASS_CONFIG_IOS_SPEAKER, 1);
  BASS_SetConfig(BASS_CONFIG_NET_PREBUF, 0);

  DefaultSoundEngineClass := TSoundEngine_Bass;
except
end;

finalization

if Bass_Available then
    Bass_Free;

end.
