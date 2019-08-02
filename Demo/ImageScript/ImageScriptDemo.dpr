program ImageScriptDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  ImageScriptFrm in 'ImageScriptFrm.pas' {ImageScriptForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TImageScriptForm, ImageScriptForm);
  Application.Run;
end.
