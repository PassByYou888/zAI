program Face_Det_GPU;

uses
  System.StartUpCopy,
  FMX.Forms,
  Face_DetFrm_GPU in 'Face_DetFrm_GPU.pas' {Face_DetForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFace_DetForm, Face_DetForm);
  Application.Run;
end.
