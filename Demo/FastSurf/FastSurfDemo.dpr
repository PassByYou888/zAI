program FastSurfDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  surfFrm in 'surfFrm.pas' {surfForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TsurfForm, surfForm);
  Application.Run;
end.
