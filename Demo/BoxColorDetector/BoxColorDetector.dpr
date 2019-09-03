program BoxColorDetector;

uses
  System.StartUpCopy,
  FMX.Forms,
  BoxColorDetectorMainFrm in 'BoxColorDetectorMainFrm.pas' {BoxColorDetectorMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBoxColorDetectorMainForm, BoxColorDetectorMainForm);
  Application.Run;
end.
