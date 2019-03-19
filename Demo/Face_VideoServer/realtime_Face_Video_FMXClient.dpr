program realtime_Face_Video_FMXClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  realtime_FACE_Video_FMXClientFrm in 'realtime_FACE_Video_FMXClientFrm.pas' {realtime_Face_Video_FMXClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Trealtime_Face_Video_FMXClientForm, realtime_Face_Video_FMXClientForm);
  Application.Run;
end.
