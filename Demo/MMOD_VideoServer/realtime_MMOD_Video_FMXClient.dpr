program realtime_MMOD_Video_FMXClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  realtime_MMOD_Video_FMXClientFrm in 'realtime_MMOD_Video_FMXClientFrm.pas' {realtime_MMOD_Video_FMXClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Trealtime_MMOD_Video_FMXClientForm, realtime_MMOD_Video_FMXClientForm);
  Application.Run;
end.
