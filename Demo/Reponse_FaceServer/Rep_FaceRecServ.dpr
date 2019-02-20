program Rep_FaceRecServ;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  CoreClasses,
  DoStatusIO,
  zAI_Common,
  zAI_Reponse_FaceClient in 'zAI_Reponse_FaceClient.pas',
  zAI_Reponse_FaceServer in 'zAI_Reponse_FaceServer.pas';

procedure RunServ;
var
  fs: TReponse_FaceServer;
begin
  fs := TReponse_FaceServer.Create;
  if fs.StartService('0.0.0.0', 8975) then
    begin
      DoStatus('face server listen 0.0.0.0:8975 successed.');
      while true do
        begin
          fs.Progress;
          if fs.Count = 0 then
              CheckThreadSynchronize(100)
          else
              CheckThreadSynchronize(1);
        end;
    end
  else
      DoStatus('face server listen 0.0.0.0:8975 failed.');
end;

begin
  ReadAIConfig;
  RunServ;

end.
