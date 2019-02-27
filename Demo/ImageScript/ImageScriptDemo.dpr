program ImageScriptDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  ImageScriptFrm in 'ImageScriptFrm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
