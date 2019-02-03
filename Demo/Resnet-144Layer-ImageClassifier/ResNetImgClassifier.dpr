program ResNetImgClassifier;

uses
  System.StartUpCopy,
  FMX.Forms,
  ResNetImgClassifierFrm in 'ResNetImgClassifierFrm.pas' {ResNetImgClassifierForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TResNetImgClassifierForm, ResNetImgClassifierForm);
  Application.Run;
end.
