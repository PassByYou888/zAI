program realtime_OD_videoserv;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  CoreClasses,
  DoStatusIO,
  PhysicsIO,
  zAI,
  zAI_Common,
  zAI_RealTime_OD_VideoServer in 'zAI_RealTime_OD_VideoServer.pas',
  zAI_RealTime_OD_VideoClient in 'zAI_RealTime_OD_VideoClient.pas';

procedure RunServ;
var
  rt_od_video_serv: TRealTime_OD_VideoServer;
begin
  rt_od_video_serv := TRealTime_OD_VideoServer.Create(TPhysicsServer.Create, TPhysicsServer.Create);

  if rt_od_video_serv.RecvTunnel.StartService('0.0.0.0', 7877)
    and
    rt_od_video_serv.SendTunnel.StartService('0.0.0.0', 7876) then
    begin
      DoStatus('listen service for realtime OD video. recv:7877, send:7876');
      while true do
        begin
          rt_od_video_serv.Progress;
          if rt_od_video_serv.TotalLinkCount > 0 then
              CoreClasses.CheckThreadSynchronize(1)
          else
              CoreClasses.CheckThreadSynchronize(100)
        end;
    end;
end;

begin
  ReadAIConfig;
  if zAI.Prepare_AI_Engine() = nil then
      raiseInfo('init ai engine failed.');
  RunServ;

end.
