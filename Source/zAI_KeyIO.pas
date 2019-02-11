{ ****************************************************************************** }
{ * AI Key IO(platform compatible)                                             * }
{ * by QQ 600585@qq.com                                                        * }
{ ****************************************************************************** }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit zAI_KeyIO;

{$INCLUDE zDefine.inc}

interface

uses SysUtils,
  DateUtils,
  CoreClasses,
  PascalStrings,
  UnicodeMixedLib,
  DoStatusIO,
  DataFrameEngine,
  PhysicsIO,
  CommunicationFramework;

type
  TAI_Key = array [0 .. 39] of Byte;

function AIKey(key: TAI_Key): TAI_Key;
procedure AIKeyState(var expire: SystemString; var OD_key, SP_key, MetricDNN_key, MMOD_key, RNIC_key: Boolean);

implementation


uses zAI_Common;

type
  TGetKeyServer_Remote = class
  public
    ProductID: TPascalString;
    UserKey: TPascalString;
    key: TAI_Key;
    ResultKey: TAI_Key;
    Tunnel: TPhysicsClient;
    expire: SystemString;
    OD_key, SP_key, MetricDNN_key, MMOD_key, RNIC_key: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure QueryAIKey;
    procedure GetKeyState;
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
  OD_key := False;
  SP_key := False;
  MetricDNN_key := False;
  MMOD_key := False;
  RNIC_key := False;
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
  sendDE := TDataFrameEngine.Create;
  ResultDE := TDataFrameEngine.Create;

  try
    if not Tunnel.RemoteInited then
      begin
        DoStatus('open key server "%s".', [AI_Key_Server_Host.Text]);
        tk := GetTimeTick();

        while not Tunnel.Connect(AI_Key_Server_Host, AI_Key_Server_Port) do
          begin
            DoStatus('key server "%s" no reponse.', [AI_Key_Server_Host.Text]);
            TCoreClassThread.Sleep(100);
            if GetTimeTick() - tk > 5 * C_Tick_Second then
                exit;
          end;
      end;

    sendDE.WriteString(ProductID);
    sendDE.WriteString(UserKey);
    sendDE.write(key[0], SizeOf(TAI_Key));
    Tunnel.WaitSendStreamCmd('QueryUserAndAIKey', sendDE, ResultDE, 5000);
    if ResultDE.Count = 0 then
        exit;
    if ResultDE.Reader.ReadBool() then
        ResultDE.Reader.read(ResultKey[0], SizeOf(TAI_Key))
    else
        DoStatus(ResultDE.Reader.ReadString());
    Tunnel.Progress;
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
  sendDE := TDataFrameEngine.Create;
  ResultDE := TDataFrameEngine.Create;

  try
    if not Tunnel.RemoteInited then
      begin
        DoStatus('open key server "%s".', [AI_Key_Server_Host.Text]);
        tk := GetTimeTick();

        while not Tunnel.Connect(AI_Key_Server_Host, AI_Key_Server_Port) do
          begin
            DoStatus('key server "%s" no reponse.', [AI_Key_Server_Host.Text]);
            TCoreClassThread.Sleep(100);
            if GetTimeTick() - tk > 15 * C_Tick_Second then
                exit;
          end;
      end;

    sendDE.WriteString(UserKey);
    Tunnel.WaitSendStreamCmd('GetKeyState', sendDE, ResultDE, 5000);
    if ResultDE.Count = 0 then
        exit;
    if ResultDE.Reader.ReadBool() then
      begin
        expire := ResultDE.Reader.ReadString();
        OD_key := ResultDE.Reader.ReadBool();
        SP_key := ResultDE.Reader.ReadBool();
        MetricDNN_key := ResultDE.Reader.ReadBool();
        MMOD_key := ResultDE.Reader.ReadBool();
        RNIC_key := ResultDE.Reader.ReadBool();
      end;
    Tunnel.Progress;
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

initialization

finalization

end.
