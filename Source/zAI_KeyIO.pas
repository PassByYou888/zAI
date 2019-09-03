{ ****************************************************************************** }
{ * AI Key IO(platform compatible)                                             * }
{ * by QQ 600585@qq.com                                                        * }
{ ****************************************************************************** }
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
unit zAI_KeyIO;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, Classes,
  DateUtils,
  CoreClasses,
  PascalStrings,
  UnicodeMixedLib,
  DoStatusIO,
  DataFrameEngine,
  PhysicsIO,
  CommunicationFramework;

type
  TAI_Key = array [0 .. 95] of Byte;

function AIKey(key: TAI_Key): TAI_Key;
procedure AIKeyState(var expire: SystemString; var OD_key, SP_key, MetricDNN_key, MMOD_key, RNIC_key: Boolean); overload;
procedure AIKeyState(var expire: SystemString; var SURF_key, OD_key, SP_key, MetricDNN_key, LMetricDNN_key, MMOD_key, RNIC_key, LRNIC_key, GDCNIC_key, GNIC_key, VideoTracker_key, SS_key: Boolean); overload;
function AIKeyInfo(): SystemString;
function AIGetFreeKey(): SystemString;

implementation


uses zAI_Common;

const
  C_Key_TimeOut = 20 * C_Tick_Second;

type
  TGetKeyServer_Remote = class(TCoreClassObject)
  public
    ProductID: TPascalString;
    UserKey: TPascalString;
    key: TAI_Key;
    ResultKey: TAI_Key;
    Tunnel: TPhysicsClient;
    expire: SystemString;
    SURF_key, OD_key, SP_key, MetricDNN_key, LMetricDNN_key, MMOD_key, RNIC_key, LRNIC_key, GDCNIC_key, GNIC_key, VideoTracker_key, SS_key: Boolean;
    KeyInfo: SystemString;
    constructor Create;
    destructor Destroy; override;
    procedure QueryAIKey;
    procedure GetKeyState;
    procedure DecodeKeyInfo;
    procedure GetFreeKey;
  end;

constructor TGetKeyServer_Remote.Create;
begin
  inherited Create;
  ProductID := AI_ProductID;
  UserKey := AI_UserKey;
  FillPtrByte(@key[0], SizeOf(TAI_Key), 0);
  FillPtrByte(@ResultKey[0], SizeOf(TAI_Key), 0);
  Tunnel := TPhysicsClient.Create;
  Tunnel.SwitchMaxSecurity;
  Tunnel.QuietMode := True;
  expire := DateToStr(umlNow());
  SURF_key := False;
  OD_key := False;
  SP_key := False;
  MetricDNN_key := False;
  LMetricDNN_key := False;
  MMOD_key := False;
  RNIC_key := False;
  LRNIC_key := False;
  GDCNIC_key := False;
  GNIC_key := False;
  VideoTracker_key := False;
  SS_key := False;
  KeyInfo := '';
end;

destructor TGetKeyServer_Remote.Destroy;
begin
  Tunnel.Disconnect;
  disposeObject(Tunnel);
  inherited Destroy;
end;

procedure TGetKeyServer_Remote.QueryAIKey;
var
  sendDE, ResultDE: TDataFrameEngine;
  tk: TTimeTick;
begin
  if TCoreClassThread.CurrentThread.ThreadID <> MainThreadID then
    begin
      DoStatus('Z-AI Work only on MainThread.');
      exit;
    end;

  sendDE := TDataFrameEngine.Create;
  ResultDE := TDataFrameEngine.Create;

  try
    if not Tunnel.RemoteInited then
      begin
        tk := GetTimeTick();

        while not Tunnel.Connect(AI_Key_Server_Host, AI_Key_Server_Port) do
          begin
            Tunnel.Progress;
            TCoreClassThread.Sleep(10);
            if GetTimeTick() - tk > 1000 then
              begin
                DoStatus('Unable to connect to license server %s:%d', [AI_Key_Server_Host.Text, AI_Key_Server_Port]);
                exit;
              end;
          end;
      end;

    sendDE.WriteString(ProductID);
    sendDE.WriteString(UserKey);
    sendDE.write(key[0], SizeOf(TAI_Key));
    Tunnel.WaitSendStreamCmd('QueryUserAndAIKey', sendDE, ResultDE, C_Key_TimeOut);
    if ResultDE.Count > 0 then
      begin
        if ResultDE.Reader.ReadBool() then
            ResultDE.Reader.read(ResultKey[0], SizeOf(TAI_Key))
        else
            DoStatus(ResultDE.Reader.ReadString());
      end;
    Tunnel.Disconnect;
    Tunnel.Progress;
  except
  end;
  disposeObject([sendDE, ResultDE]);
end;

procedure TGetKeyServer_Remote.GetKeyState;
var
  sendDE, ResultDE: TDataFrameEngine;
  tk: TTimeTick;
begin
  if TCoreClassThread.CurrentThread.ThreadID <> MainThreadID then
    begin
      DoStatus('Z-AI Work only on MainThread.');
      exit;
    end;

  sendDE := TDataFrameEngine.Create;
  ResultDE := TDataFrameEngine.Create;

  try
    if not Tunnel.RemoteInited then
      begin
        tk := GetTimeTick();

        while not Tunnel.Connect(AI_Key_Server_Host, AI_Key_Server_Port) do
          begin
            Tunnel.Progress;
            TCoreClassThread.Sleep(10);
            if GetTimeTick() - tk > 1000 then
              begin
                DoStatus('Unable to connect to license server %s:%d', [AI_Key_Server_Host.Text, AI_Key_Server_Port]);
                exit;
              end;
          end;
      end;

    sendDE.WriteString(UserKey);
    Tunnel.WaitSendStreamCmd('GetKeyState', sendDE, ResultDE, C_Key_TimeOut);
    if ResultDE.Count > 0 then
      if ResultDE.Reader.ReadBool() then
        begin
          expire := ResultDE.Reader.ReadString();

          SURF_key := ResultDE.Reader.ReadBool();
          OD_key := ResultDE.Reader.ReadBool();
          SP_key := ResultDE.Reader.ReadBool();
          MetricDNN_key := ResultDE.Reader.ReadBool();
          LMetricDNN_key := ResultDE.Reader.ReadBool();
          MMOD_key := ResultDE.Reader.ReadBool();
          RNIC_key := ResultDE.Reader.ReadBool();
          LRNIC_key := ResultDE.Reader.ReadBool();
          GDCNIC_key := ResultDE.Reader.ReadBool();
          GNIC_key := ResultDE.Reader.ReadBool();
          VideoTracker_key := ResultDE.Reader.ReadBool();
          SS_key := ResultDE.Reader.ReadBool();
        end;
    Tunnel.Disconnect;
    Tunnel.Progress;
  except
  end;
  disposeObject([sendDE, ResultDE]);
end;

procedure TGetKeyServer_Remote.DecodeKeyInfo;
var
  sendDE, ResultDE: TDataFrameEngine;
  tk: TTimeTick;
begin
  if TCoreClassThread.CurrentThread.ThreadID <> MainThreadID then
    begin
      DoStatus('Z-AI Work only on MainThread.');
      exit;
    end;

  sendDE := TDataFrameEngine.Create;
  ResultDE := TDataFrameEngine.Create;

  try
    if not Tunnel.RemoteInited then
      begin
        tk := GetTimeTick();

        while not Tunnel.Connect(AI_Key_Server_Host, AI_Key_Server_Port) do
          begin
            Tunnel.Progress;
            TCoreClassThread.Sleep(10);
            if GetTimeTick() - tk > 1000 then
              begin
                DoStatus('Unable to connect to license server %s:%d', [AI_Key_Server_Host.Text, AI_Key_Server_Port]);
                exit;
              end;
          end;
      end;

    sendDE.WriteString(UserKey);
    Tunnel.WaitSendStreamCmd('DecodeKeyInfo', sendDE, ResultDE, C_Key_TimeOut);
    if ResultDE.Count > 0 then
      if ResultDE.Reader.ReadBool() then
        begin
          KeyInfo := ResultDE.Reader.ReadString();
        end;

    Tunnel.Disconnect;
    Tunnel.Progress;
  except
  end;
  disposeObject([sendDE, ResultDE]);
end;

procedure TGetKeyServer_Remote.GetFreeKey;
var
  sendDE, ResultDE: TDataFrameEngine;
  tk: TTimeTick;
begin
  if TCoreClassThread.CurrentThread.ThreadID <> MainThreadID then
    begin
      DoStatus('Z-AI Work only on MainThread.');
      exit;
    end;

  sendDE := TDataFrameEngine.Create;
  ResultDE := TDataFrameEngine.Create;

  try
    if not Tunnel.RemoteInited then
      begin
        tk := GetTimeTick();

        while not Tunnel.Connect(AI_Key_Server_Host, AI_Key_Server_Port) do
          begin
            Tunnel.Progress;
            TCoreClassThread.Sleep(10);
            if GetTimeTick() - tk > 1000 then
              begin
                DoStatus('Unable to connect to license server %s:%d', [AI_Key_Server_Host.Text, AI_Key_Server_Port]);
                exit;
              end;
          end;
      end;

    Tunnel.WaitSendStreamCmd('GetFreeKey', sendDE, ResultDE, C_Key_TimeOut);
    if ResultDE.Count > 0 then
        KeyInfo := ResultDE.Reader.ReadString();

    Tunnel.Disconnect;
    Tunnel.Progress;
  except
  end;
  disposeObject([sendDE, ResultDE]);
end;

function AIKey(key: TAI_Key): TAI_Key;
var
  K_Tunnel: TGetKeyServer_Remote;
begin
  K_Tunnel := TGetKeyServer_Remote.Create;
  K_Tunnel.key := key;
  FillPtrByte(@K_Tunnel.ResultKey[0], SizeOf(TAI_Key), 0);
  K_Tunnel.QueryAIKey();
  Result := K_Tunnel.ResultKey;
  disposeObject(K_Tunnel);
end;

procedure AIKeyState(var expire: SystemString; var OD_key, SP_key, MetricDNN_key, MMOD_key, RNIC_key: Boolean);
var
  K_Tunnel: TGetKeyServer_Remote;
begin
  K_Tunnel := TGetKeyServer_Remote.Create;
  K_Tunnel.GetKeyState();
  expire := K_Tunnel.expire;
  OD_key := K_Tunnel.OD_key;
  SP_key := K_Tunnel.SP_key;
  MetricDNN_key := K_Tunnel.MetricDNN_key;
  MMOD_key := K_Tunnel.MMOD_key;
  RNIC_key := K_Tunnel.RNIC_key;
  disposeObject(K_Tunnel);
end;

procedure AIKeyState(var expire: SystemString; var SURF_key, OD_key, SP_key, MetricDNN_key, LMetricDNN_key, MMOD_key, RNIC_key, LRNIC_key, GDCNIC_key, GNIC_key, VideoTracker_key, SS_key: Boolean);
var
  K_Tunnel: TGetKeyServer_Remote;
begin
  K_Tunnel := TGetKeyServer_Remote.Create;
  K_Tunnel.GetKeyState();
  expire := K_Tunnel.expire;
  SURF_key := K_Tunnel.SURF_key;
  OD_key := K_Tunnel.OD_key;
  SP_key := K_Tunnel.SP_key;
  MetricDNN_key := K_Tunnel.MetricDNN_key;
  LMetricDNN_key := K_Tunnel.LMetricDNN_key;
  MMOD_key := K_Tunnel.MMOD_key;
  RNIC_key := K_Tunnel.RNIC_key;
  LRNIC_key := K_Tunnel.LRNIC_key;
  GDCNIC_key := K_Tunnel.GDCNIC_key;
  GNIC_key := K_Tunnel.GNIC_key;
  VideoTracker_key := K_Tunnel.VideoTracker_key;
  SS_key := K_Tunnel.SS_key;
  disposeObject(K_Tunnel);
end;

function AIKeyInfo(): SystemString;
var
  K_Tunnel: TGetKeyServer_Remote;
begin
  K_Tunnel := TGetKeyServer_Remote.Create;
  K_Tunnel.DecodeKeyInfo();
  Result := K_Tunnel.KeyInfo;
  disposeObject(K_Tunnel);
end;

function AIGetFreeKey(): SystemString;
var
  K_Tunnel: TGetKeyServer_Remote;
begin
  K_Tunnel := TGetKeyServer_Remote.Create;
  K_Tunnel.GetFreeKey();
  Result := K_Tunnel.KeyInfo;
  disposeObject(K_Tunnel);
end;

initialization

finalization

end.
