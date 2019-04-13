program LargeMetricImgClassifier;

uses
  System.StartUpCopy,
  FMX.Forms,
  LargeMetricImgClassifierFrm in 'LargeMetricImgClassifierFrm.pas' {LargeMetricImgClassifierForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TLargeMetricImgClassifierForm, LargeMetricImgClassifierForm);
  Application.Run;
end.
