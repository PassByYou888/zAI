unit zAI_RealTime_MMOD_VideoClient;

interface

uses Types, CoreClasses, PascalStrings, UnicodeMixedLib, DoStatusIO, DataFrameEngine,
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  TextDataEngine, ListEngine, zDrawEngine, MemoryRaster, MemoryStream64, Geometry2DUnit, Geometry3DUnit,
  CommunicationFramework, CommunicationFrameworkDoubleTunnelIO_NoAuth;

type
  TMMOD_VideoIO_ = class(TPeerIOUserSpecial)
  public
    VideoFrames: TMemoryStream64List;
    constructor Create(AOwner: TPeerIO); override;
    destructor Destroy; override;

    procedure Progress; override;
    procedure ClearVideoFrames;
  end;

  TMMOD_Video_Data = record
    r: TRectV2;
    confidence: Double;
    token: TPascalString;
  end;

{$IFDEF FPC}

  TMMOD_Video_Info = specialize TGenericsList<TMMOD_Video_Data>;
{$ELSE FPC}
  TMMOD_Video_Info = TGenericsList<TMMOD_Video_Data>;
{$ENDIF FPC}
  TRealTime_MMOD_VideoClient = class;

  TOn_MMOD_Result = procedure(Sender: TRealTime_MMOD_VideoClient; video_stream: TMemoryStream64; video_info: TMMOD_Video_Info) of object;

  TRealTime_MMOD_VideoClient = class(TCommunicationFramework_DoubleTunnelClient_NoAuth)
  private
    procedure cmd_VideoBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    procedure cmd_VideoInfo(Sender: TPeerIO; InData: TDataFrameEngine);
  public
    On_MMOD_Result: TOn_MMOD_Result;
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
    destructor Destroy; override;

    procedure Progress; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    procedure Input_MMOD(r: TMemoryRaster);
  end;

implementation

procedure TMMOD_VideoIO_.ClearVideoFrames;
var
  i: Integer;
begin
  for i := 0 to VideoFrames.Count - 1 do
      DisposeObject(VideoFrames[i]);
  VideoFrames.Clear;
end;

constructor TMMOD_VideoIO_.Create(AOwner: TPeerIO);
begin
  inherited Create(AOwner);
  VideoFrames := TMemoryStream64List.Create;
end;

destructor TMMOD_VideoIO_.Destroy;
begin
  ClearVideoFrames;
  DisposeObject(VideoFrames);
  inherited Destroy;
end;

procedure TMMOD_VideoIO_.Progress;
begin
  inherited Progress;
end;

procedure TRealTime_MMOD_VideoClient.cmd_VideoBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.WritePtr(InData, DataSize);
  m64.Position := 0;

  TMMOD_VideoIO_(Sender.UserSpecial).VideoFrames.Add(m64);
end;

procedure TRealTime_MMOD_VideoClient.cmd_VideoInfo(Sender: TPeerIO; InData: TDataFrameEngine);
var
  MMOD_data: TMMOD_Video_Data;
  video_info: TMMOD_Video_Info;
  i, c: Integer;
begin
  video_info := TMMOD_Video_Info.Create;
  c := InData.Reader.ReadInteger;
  for i := 0 to c - 1 do
    begin
      MMOD_data.r := InData.Reader.ReadRectV2;
      MMOD_data.confidence := InData.Reader.ReadDouble;
      MMOD_data.token := InData.Reader.ReadString;
      video_info.Add(MMOD_data);
    end;

  if Assigned(On_MMOD_Result) then
      On_MMOD_Result(Self, TMMOD_VideoIO_(Sender.UserSpecial).VideoFrames.Last, video_info);

  DisposeObject(video_info);
  TMMOD_VideoIO_(Sender.UserSpecial).ClearVideoFrames;
end;

constructor TRealTime_MMOD_VideoClient.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
begin
  inherited Create(ARecvTunnel, ASendTunnel);
  RecvTunnel.UserSpecialClass := TMMOD_VideoIO_;
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

  On_MMOD_Result := nil;
end;

destructor TRealTime_MMOD_VideoClient.Destroy;
begin
  inherited Destroy;
end;

procedure TRealTime_MMOD_VideoClient.Progress;
begin
  inherited Progress;
end;

procedure TRealTime_MMOD_VideoClient.RegisterCommand;
begin
  inherited RegisterCommand;
  RecvTunnel.RegisterCompleteBuffer('VideoBuffer').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_VideoBuffer;
  RecvTunnel.RegisterDirectStream('VideoInfo').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_VideoInfo;
end;

procedure TRealTime_MMOD_VideoClient.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  RecvTunnel.UnRegisted('VideoBuffer');
  RecvTunnel.UnRegisted('VideoInfo');
end;

procedure TRealTime_MMOD_VideoClient.Input_MMOD(r: TMemoryRaster);
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
