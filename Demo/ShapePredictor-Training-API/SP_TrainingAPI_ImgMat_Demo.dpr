program SP_TrainingAPI_ImgMat_Demo;

uses
  Vcl.Forms,
  SPTrainAPIDemoFrm in 'SPTrainAPIDemoFrm.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
