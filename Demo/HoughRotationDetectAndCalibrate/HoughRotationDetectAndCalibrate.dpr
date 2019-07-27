program HoughRotationDetectAndCalibrate;

uses
  System.StartUpCopy,
  FMX.Forms,
  HoughRotationDetectAndCalibrateMainFrm in 'HoughRotationDetectAndCalibrateMainFrm.pas' {HoughRotationDetectAndCalibrateMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(THoughRotationDetectAndCalibrateMainForm, HoughRotationDetectAndCalibrateMainForm);
  Application.Run;
end.
