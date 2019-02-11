program FastSurfDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  surfFrm in 'surfFrm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
