program ResNetImgClassifier2;

uses
  System.StartUpCopy,
  FMX.Forms,
  ResNetImgClassifierFrm2 in 'ResNetImgClassifierFrm2.pas' {ResNetImgClassifierForm2},
  ShowImageFrm in '..\Reponse_FaceServer\ShowImageFrm.pas' {ShowImageForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TResNetImgClassifierForm2, ResNetImgClassifierForm2);
  Application.Run;
end.
