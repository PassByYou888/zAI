program Face_Metric_ResNet_TrainingAPI_ImgMat_Demo;

uses
  Vcl.Forms,
  Face_Metric_ResNet_TrainAPIDemoFrm in 'Face_Metric_ResNet_TrainAPIDemoFrm.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
