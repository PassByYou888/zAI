program AISetFormatExtract;

uses
  System.StartUpCopy,
  FMX.Forms,
  AISetFormatExtractMainFrm in 'AISetFormatExtractMainFrm.pas' {AISetFormatExtractMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAISetFormatExtractMainForm, AISetFormatExtractMainForm);
  Application.Run;
end.
