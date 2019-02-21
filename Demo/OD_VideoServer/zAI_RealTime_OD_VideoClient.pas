unit zAI_RealTime_OD_VideoClient;

interface

uses Types, CoreClasses, PascalStrings, UnicodeMixedLib, DoStatusIO, DataFrameEngine,
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  TextDataEngine, ListEngine, zDrawEngine, MemoryRaster, MemoryStream64, Geometry2DUnit, Geometry3DUnit,
  CommunicationFramework, CommunicationFrameworkDoubleTunnelIO_NoAuth;

type
  TOD_VideoIO_ = class(TPeerIOUserSpecial)
  public
    VideoFrames: TMemoryStream64List;
    constructor Create(AOwner: TPeerIO); override;
    destructor Destroy; override;

    procedure Progress; override;
    procedure ClearVideoFrames;
  end;

  TOD_Video_Data = record
    r: TRect;
    confidence: Double;
  end;

{$IFDEF FPC}

  TOD_Video_Info = specialize TGenericsList<TOD_Video_Data>;
{$ELSE FPC}
  TOD_Video_Info = TGenericsList<TOD_Video_Data>;
{$ENDIF FPC}
  TRealTime_OD_VideoClient = class;

  TOnOD_Result = procedure(Sender: TRealTime_OD_VideoClient; video_stream: TMemoryStream64; video_info: TOD_Video_Info) of object;

  TRealTime_OD_VideoClient = class(TCommunicationFramework_DoubleTunnelClient_NoAuth)
  private
    procedure cmd_VideoBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    procedure cmd_VideoInfo(Sender: TPeerIO; InData: TDataFrameEngine);
  public
    OnOD_Result: TOnOD_Result;
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
    destructor Destroy; override;

    procedure Progress; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    procedure Input_OD(r: TMemoryRaster);
  end;

implementation

procedure TOD_VideoIO_.ClearVideoFrames;
var
  i: Integer;
begin
  for i := 0 to VideoFrames.Count - 1 do
      DisposeObject(VideoFrames[i]);
  VideoFrames.Clear;
end;

constructor TOD_VideoIO_.Create(AOwner: TPeerIO);
begin
  inherited Create(AOwner);
  VideoFrames := TMemoryStream64List.Create;
end;

destructor TOD_VideoIO_.Destroy;
begin
  ClearVideoFrames;
  DisposeObject(VideoFrames);
  inherited Destroy;
end;

procedure TOD_VideoIO_.Progress;
begin
  inherited Progress;
end;

procedure TRealTime_OD_VideoClient.cmd_VideoBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.WritePtr(InData, DataSize);
  m64.Position := 0;

  TOD_VideoIO_(Sender.UserSpecial).VideoFrames.Add(m64);
end;

procedure TRealTime_OD_VideoClient.cmd_VideoInfo(Sender: TPeerIO; InData: TDataFrameEngine);
var
  OD_data: TOD_Video_Data;
  video_info: TOD_Video_Info;
  i, c: Integer;
begin
  video_info := TOD_Video_Info.Create;
  c := InData.Reader.ReadInteger;
  for i := 0 to c - 1 do
    begin
      OD_data.r.Left := InData.Reader.ReadInteger;
      OD_data.r.Top := InData.Reader.ReadInteger;
      OD_data.r.Right := InData.Reader.ReadInteger;
      OD_data.r.Bottom := InData.Reader.ReadInteger;
      OD_data.confidence := InData.Reader.ReadDouble;
      video_info.Add(OD_data);
    end;

  if Assigned(OnOD_Result) then
      OnOD_Result(Self, TOD_VideoIO_(Sender.UserSpecial).VideoFrames.Last, video_info);

  DisposeObject(video_info);
  TOD_VideoIO_(Sender.UserSpecial).ClearVideoFrames;
end;

constructor TRealTime_OD_VideoClient.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
begin
  inherited Create(ARecvTunnel, ASendTunnel);
  RecvTunnel.UserSpecialClass := TOD_VideoIO_;
  RecvTunnel.MaxCompleteBufferSize := 8 * 1024 * 1024; // 8M complete buffer
  SwitchAsMaxPerformance;

  // max network performance
  SendTunnel.SendDataCompressed := True;
  RecvTunnel.SendDataCompressed := True;
  RecvTunnel.CompleteBufferCompressed := False;
  SendTunnel.CompleteBufferCompressed := False;
  RecvTunnel.SequencePacketActivted := False;
  SendTunnel.SequencePacketActivted := False;

  // disable print state
  SendTunnel.PrintParams['VideoBuffer'] := False;
  SendTunnel.PrintParams['OD'] := False;
  RecvTunnel.PrintParams['VideoBuffer'] := False;
  RecvTunnel.PrintParams['VideoInfo'] := False;

  RegisterCommand;

  OnOD_Result := nil;
end;

destructor TRealTime_OD_VideoClient.Destroy;
begin
  inherited Destroy;
end;

procedure TRealTime_OD_VideoClient.Progress;
begin
  inherited Progress;
end;

procedure TRealTime_OD_VideoClient.RegisterCommand;
begin
  inherited RegisterCommand;
  RecvTunnel.RegisterCompleteBuffer('VideoBuffer').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_VideoBuffer;
  RecvTunnel.RegisterDirectStream('VideoInfo').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_VideoInfo;
end;

procedure TRealTime_OD_VideoClient.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  RecvTunnel.UnRegisted('VideoBuffer');
  RecvTunnel.UnRegisted('VideoInfo');
end;

procedure TRealTime_OD_VideoClient.Input_OD(r: TMemoryRaster);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  r.SaveToJpegRGBStream(m64, 50);
  SendTunnel.SendCompleteBuffer('VideoBuffer', m64.Memory, m64.Size, True);
  m64.DiscardMemory;
  DisposeObject(m64);

  SendTunnel.SendDirectStreamCmd('OD');
end;

end.
