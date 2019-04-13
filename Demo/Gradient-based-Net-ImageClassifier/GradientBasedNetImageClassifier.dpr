program GradientBasedNetImageClassifier;

uses
  System.StartUpCopy,
  FMX.Forms,
  GradientBasedNetImageClassifierFrm in 'GradientBasedNetImageClassifierFrm.pas' {GradientBasedNetImageClassifierForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGradientBasedNetImageClassifierForm, GradientBasedNetImageClassifierForm);
  Application.Run;
end.
