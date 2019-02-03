program Face_Rec_GPU;

uses
  System.StartUpCopy,
  FMX.Forms,
  FaceRec_GPU_DemoFrm in 'FaceRec_GPU_DemoFrm.pas' {FaceRecForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFaceRecForm, FaceRecForm);
  Application.Run;
end.
