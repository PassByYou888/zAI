program zDrawEngine_HelloWorld;

uses
  System.StartUpCopy,
  FMX.Forms,
  zDrawEngineFrm in 'zDrawEngineFrm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
