program Rep_FaceRecCli;

uses
  System.StartUpCopy,
  FMX.Forms,
  FaceClientFrm in 'FaceClientFrm.pas' {FaceClientForm},
  ShowImageFrm in 'ShowImageFrm.pas' {ShowImageForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFaceClientForm, FaceClientForm);
  Application.Run;
end.
