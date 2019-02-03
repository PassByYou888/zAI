program Face_Rec_cpu;

uses
  System.StartUpCopy,
  FMX.Forms,
  FaceRec_CPU_DemoFrm in 'FaceRec_CPU_DemoFrm.pas' {FaceRecForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFaceRecForm, FaceRecForm);
  Application.Run;
end.
