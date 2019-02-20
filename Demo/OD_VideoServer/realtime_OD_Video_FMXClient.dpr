program realtime_OD_Video_FMXClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  realtime_OD_Video_FMXClientFrm in 'realtime_OD_Video_FMXClientFrm.pas' {realtime_OD_Video_FMXClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Trealtime_OD_Video_FMXClientForm, realtime_OD_Video_FMXClientForm);
  Application.Run;
end.
